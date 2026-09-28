require 'json'
require 'net/http'

email, key_path, details_limit, nearby_limit = ARGV
api_key = File.read(key_path).strip
user = User.find_by!(email: email)
details_limit = Integer(details_limit || '500')
nearby_limit = Integer(nearby_limit || '50')
generic_visits = ['Unknown', 'Searched Address', 'Frequent place', 'Aliased Location', 'Unknown Location']
ignored_types = %w[route locality political sublocality neighborhood postal_code administrative_area_level_1
                   administrative_area_level_2 country plus_code intersection]
counts = Hash.new(0)

class QuotaStop < StandardError; end

places_request = lambda do |request, field_mask|
  request['X-Goog-Api-Key'] = api_key
  request['X-Goog-FieldMask'] = field_mask
  request['Content-Type'] = 'application/json'
  attempts = 0
  begin
    response = Net::HTTP.start('places.googleapis.com', 443, use_ssl: true, open_timeout: 10, read_timeout: 20) do |http|
      http.request(request)
    end
  rescue Net::OpenTimeout, Net::ReadTimeout
    attempts += 1
    raise if attempts > 2

    sleep 5 * attempts
    retry
  end
  code = response.code.to_i
  raise QuotaStop, "HTTP #{code}" if code == 429 || code >= 500 || code == 403

  [code, JSON.parse(response.body.presence || '{}')]
end

rename = lambda do |place, name, google|
  place.with_lock do
    next counts[:skipped_locked] += 1 if place.name_locked? || %w[Home Work].include?(place.name)

    old_name = place.name
    place.machine_named = true
    place.update!(name: name.truncate(255), geodata: place.geodata.merge('google' => google.merge('previous_name' => old_name)))
    counts[:visits_renamed] += place.visits.where(name: [old_name, *generic_visits]).update_all(name: place.name)
  end
end

mark = lambda do |place, google|
  place.update_columns(geodata: place.geodata.merge('google' => google))
end

base = user.places.where(name_locked_at: nil).where.not(name: %w[Home Work]).where("NOT geodata ? 'google'")
visit_counts = Visit.where(status: [0, 1], deleted_at: nil).group(:place_id).select('place_id, COUNT(*) AS n')

begin
  base.where("geodata->>'external_place_id' LIKE 'google:%'")
      .joins("LEFT JOIN (#{visit_counts.to_sql}) vc ON vc.place_id = places.id")
      .order(Arel.sql('COALESCE(vc.n, 0) DESC, places.id'))
      .limit(details_limit).each do |place|
    place_id = place.geodata['external_place_id'].delete_prefix('google:')
    code, body = places_request.call(
      Net::HTTP::Get.new("/v1/places/#{place_id}"), 'id,displayName,primaryType,formattedAddress'
    )
    google = { 'match' => 'details', 'place_id' => body['id'] || place_id, 'primary_type' => body['primaryType'],
               'fetched_at' => Time.current.iso8601 }.compact
    name = body.dig('displayName', 'text')
    if code == 200 && name.present?
      rename.call(place, name, google)
      counts[:details_renamed] += 1
    else
      mark.call(place, google.merge('status' => code))
      counts[:"details_#{code}"] += 1
    end
  end

  base.where("NOT geodata ? 'external_place_id'").where('places.created_at > ?', 30.days.ago)
      .order(:id).limit(nearby_limit).each do |place|
    overnight = place.visits.where(status: [0, 1]).where('duration >= 240')
                     .where("date_trunc('day', started_at) <> date_trunc('day', ended_at)").count
    if overnight * 2 >= [place.visits.count, 1].max
      mark.call(place, { 'match' => 'skipped_residential', 'fetched_at' => Time.current.iso8601 })
      next counts[:nearby_skipped_residential] += 1
    end

    request = Net::HTTP::Post.new('/v1/places:searchNearby')
    request.body = {
      locationRestriction: { circle: { center: { latitude: place.lat.to_f, longitude: place.lon.to_f }, radius: 40.0 } },
      maxResultCount: 5, rankPreference: 'DISTANCE'
    }.to_json
    code, body = places_request.call(request, 'places.id,places.displayName,places.primaryType,places.location')
    candidate = Array(body['places']).find do |p|
      p.dig('displayName', 'text').present? && !ignored_types.include?(p['primaryType'])
    end
    unless code == 200 && candidate
      mark.call(place, { 'match' => 'none', 'status' => code, 'fetched_at' => Time.current.iso8601 })
      next counts[:nearby_none] += 1
    end

    distance = Geocoder::Calculations.distance_between(
      [place.lat, place.lon], [candidate.dig('location', 'latitude'), candidate.dig('location', 'longitude')], units: :km
    ) * 1000
    rename.call(place, candidate.dig('displayName', 'text'),
                { 'match' => 'nearby', 'place_id' => candidate['id'], 'primary_type' => candidate['primaryType'],
                  'distance_m' => distance.round(1), 'fetched_at' => Time.current.iso8601 }.compact)
    counts[:nearby_renamed] += 1
  end
rescue QuotaStop, Net::OpenTimeout, Net::ReadTimeout, SocketError, SystemCallError => e
  counts[:stopped] = e.class.name
  warn "Google Places step stopped early: #{e.message}"
end

puts counts.inspect
