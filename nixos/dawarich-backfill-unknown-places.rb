limit = Integer(ARGV.fetch(0, '100'))
raise ArgumentError, 'limit must be between 1 and 100' unless (1..100).cover?(limit)

user = User.find_by!(email: 'ivanmalison@gmail.com')
cursor_file = '/var/lib/dawarich/unknown-places-backfill-cursor'
cursor = File.exist?(cursor_file) ? Integer(File.read(cursor_file).strip) : 0
generic_names = ['Unknown', 'Searched Address', 'Frequent place', 'Aliased Location']
generic_visit_names = [*generic_names, 'Unknown Location']
unknown = user.places.where(name: generic_names).where(name_locked_at: nil)
batch = unknown.where('id > ?', cursor).order(:id).limit(limit).to_a
if batch.empty?
  cursor = 0
  batch = unknown.order(:id).limit(limit).to_a
end

counts = Hash.new(0)
quiet_output = File.open(File::NULL, 'wb')
consecutive_errors = 0
batch.each do |place|
  break if consecutive_errors >= 5

  cursor = place.id
  begin
    original_stdout = $stdout
    begin
      $stdout = quiet_output
      result = Geocoding::Search.call(
        user: user, query: [place.lat, place.lon], units: :km, limit: 1, distance_sort: true
      ).first
    ensure
      $stdout = original_stdout
    end
    consecutive_errors = 0
    unless result
      counts[:no_result] += 1
      next
    end

    properties = Geocoding::ResultNormalizer.call(result)[:properties]
    name = Visits::Names::Builder.build_from_properties(properties)
    unless name.present? && (properties['name'].present? || properties['street'].present?)
      counts[:no_useful_name] += 1
      next
    end

    distance = Geocoder::Calculations.distance_between(
      [place.lat, place.lon], [result.latitude, result.longitude], units: :km
    ) * 1000
    if distance > 100
      counts[:too_far] += 1
      next
    end

    place.with_lock do
      next unless generic_names.include?(place.name) && !place.name_locked?

      place.machine_named = true
      place.update!(
        name: name.truncate(255),
        city: properties['city'].presence || place.city,
        country: properties['country'].presence || place.country,
        geodata: place.geodata.merge(result.data)
      )
      counts[:visits_renamed] += place.visits.where(name: generic_visit_names).update_all(name: place.name)
      counts[:places_renamed] += 1
    end
  rescue Geocoder::Error, Timeout::Error, SocketError, SystemCallError => e
    consecutive_errors += 1
    counts[:provider_errors] += 1
    Rails.logger.warn("Unknown-place backfill skipped place #{place.id}: #{e.class}")
  end
end
quiet_output.close

File.write(cursor_file, cursor.to_s) unless batch.empty?
puts({attempted: batch.length, remaining: unknown.count, **counts}.inspect)
