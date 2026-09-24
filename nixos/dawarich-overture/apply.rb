require 'json'

email, decisions_path, release = ARGV
user = User.find_by!(email: email)
generic_places = ['Unknown', 'Searched Address', 'Frequent place', 'Aliased Location']
generic_visits = [*generic_places, 'Unknown Location']
counts = Hash.new(0)

JSON.parse(File.read(decisions_path)).each do |d|
  if d['kind'] == 'place'
    place = user.places.find_by(id: d['id'])
    next counts[:skipped] += 1 unless place

    place.with_lock do
      next counts[:skipped] += 1 if place.name_locked? || %w[Home Work].include?(place.name)

      overture = { 'release' => release, 'match' => d['how'] }
      if d['how'] == 'none'
        place.update_columns(geodata: place.geodata.merge('overture' => overture))
        next counts[:places_unmatched] += 1
      end

      old_name = place.name
      place.machine_named = true
      place.update!(
        name: d['new_name'].truncate(255),
        city: d['city'].presence || place.city,
        country: place.country.presence || d['country'],
        geodata: place.geodata.merge(
          'overture' => overture.merge('id' => d['overture_id'], 'category' => d['category'],
                                       'distance_m' => d['distance_m'], 'previous_name' => old_name).compact
        )
      )
      counts[:"places_#{d['how']}"] += 1
      counts[:visits_renamed] += place.visits.where(name: [old_name, *generic_visits]).update_all(name: place.name)
    end
  else
    visit = user.visits.find_by(id: d['id'], place_id: nil, name: generic_visits)
    next counts[:skipped] += 1 unless visit

    if d['how'] == 'place'
      place = user.places.find_by(id: d['place_id'])
      next counts[:skipped] += 1 unless place && !user.visits.exists?(started_at: visit.started_at, place_id: place.id)

      visit.update!(place: place, name: place.name)
      counts[:visits_attached] += 1
    else
      visit.update!(name: d['new_name'].truncate(255))
      counts[:"visits_#{d['how']}"] += 1
    end
  end
rescue ActiveRecord::RecordInvalid => e
  counts[:errors] += 1
  warn "#{d['kind']} #{d['id']}: #{e.message}"
end

puts counts.inspect
