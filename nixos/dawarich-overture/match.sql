create or replace table ptargets as
  select * from read_csv('places.tsv', delim='\t', header=false, quote='',
    columns={'id': 'BIGINT', 'lat': 'DOUBLE', 'lon': 'DOUBLE', 'is_google': 'BOOLEAN', 'photon_poi': 'BOOLEAN', 'nvisits': 'INTEGER', 'overnight': 'DOUBLE'});
create or replace table targets as
  select 'place' kind, id, lat, lon from ptargets
  union all
  select 'visit', * from read_csv('visits.tsv', delim='\t', header=false, columns={'id': 'BIGINT', 'lat': 'DOUBLE', 'lon': 'DOUBLE'});
create or replace table all_places as
  select * from read_csv('all-places.tsv', delim='\t', header=false, columns={'id': 'BIGINT', 'lat': 'DOUBLE', 'lon': 'DOUBLE'});

create or replace macro dist_m(lat1, lon1, lat2, lon2) as
  6371000 * 2 * asin(sqrt(pow(sin(radians(lat2 - lat1) / 2), 2) + cos(radians(lat1)) * cos(radians(lat2)) * pow(sin(radians(lon2 - lon1) / 2), 2)));
create or replace macro is_office(cat) as regexp_matches(coalesce(cat, ''), '(office|architect|software|legal|attorney|law_firm|consult|agency|contractor|accountant|financial|media|broadcast|real_estate|marketing|insurance|business|chiropract|dentist|orthodont|doctor|physician|counsel|therap|lawyer|notary|printing|cleaning|construction|engineer|manufactur|wholesale|design|advertis|photograph|event_planning|recording|remodel|delivery_service|mortgage)');
create or replace macro is_big(cat) as regexp_matches(coalesce(cat, ''), '((^|_)park($|_)|airport|stadium|arena|university|college|campus|beach|golf|ski|resort|zoo|museum|hospital|shopping_mall|shopping_center|train_station|terminal|national|trail|lake|garden|amusement|convention|casino|farm|marina|campground|theme|high_school)');
create or replace macro is_medium(cat) as regexp_matches(coalesce(cat, ''), '(supermarket|grocery|department_store|warehouse|home_improvement|hardware|wholesale_store|gym|fitness|sport|recreation|community_center|hotel|lodging|church|library|school|movie_theater|cinema|climbing|swimming|ice_skating|bowling)');
create or replace macro is_repeat(cat) as regexp_matches(coalesce(cat, ''), '(supermarket|grocery|market|gym|fitness|yoga|pilates|climbing|sport|recreation|coffee|cafe|bakery|restaurant|food|pizza|burger|sandwich|taco|sushi|ramen|noodle|diner|bar|pub|brewery|tea|juice|park|playground|school|university|college|office|church|library|pharmacy|station|charging|gas_station|community|hospital|clinic|laundr|convenience|liquor|deli|hotel|lodging|coworking|trail|beach|swimming|theater|cinema|music|venue|barber|salon|dog|pet_service|veterinar|bank|post_office)');
create or replace macro is_oneoff(cat) as regexp_matches(coalesce(cat, ''), '(jewel|shoe|cloth|boutique|fashion|apparel|lingerie|furniture|skin_care|beauty_supply|cosmetic|electronics|mobile_phone|eyewear|sunglasses|optical|wedding|car_dealer|locksmith|gift|toy|diagnostic|laborator|art_gallery|mattress|carpet|appliance|tailor|bridal|piercing|tattoo|psychic|atm)');
create or replace macro is_lodging(cat) as regexp_matches(coalesce(cat, ''), '(hotel|motel|lodging|inn|resort|hostel|bed_and_breakfast|guest_house|campground|cabin|vacation_rental|apartment|condominium|residential)');

create or replace table pairs as
  select t.kind, t.id tid, c.id cid, c."name", c.category, c.confidence, c.operating_status, dist_m(t.lat, t.lon, c.lat, c.lon) d
  from targets t join (select distinct on (id) * from cand) c on abs(t.lat - c.lat) < 0.0015 and abs(t.lon - c.lon) < 0.002
  where c."name" is not null and not regexp_matches(c."name", '(?i)flight [0-9]+|checkpoint');

create or replace table scored as
  select p.*,
    (0.3 + 0.7 * coalesce(confidence, 0))
    * case when is_office(category) then 0.35 else 1 end
    * case when category = 'lodging' and not regexp_matches(lower(p."name"), '(hotel|inn|motel|hostel|suites|lodge|resort|b&b|bed and breakfast)') then 0.5 else 1 end
    * case when operating_status = 'permanently_closed' then 0.8 else 1 end
    * case when coalesce(s.nvisits, 1) >= 10 then (case when is_oneoff(category) then 0.3 when is_repeat(category) then 1 else 0.7 end)
           when coalesce(s.nvisits, 1) >= 3 then (case when is_oneoff(category) then 0.6 when is_repeat(category) then 1 else 0.85 end)
           else 1 end
    * exp(-d / case when is_big(category) then 40 when is_medium(category) then 20 else 12 end) score,
    d <= case when is_big(category) then 120 when is_medium(category) then 60 else 30 end in_range
  from pairs p left join ptargets s on p.kind = 'place' and s.id = p.tid;

create or replace table best as
  select kind, tid, arg_max(cid, score) AS cid, arg_max("name", score) AS pname, arg_max(category, score) AS category, arg_max(d, score) AS d, max(score) AS score
  from scored where in_range group by kind, tid;

create or replace table nearaddr as
  with j as (
    select t.kind, t.id tid, a."number", a.street, a.city, a.country, dist_m(t.lat, t.lon, a.lat, a.lon) AS d
    from targets t join addr a on abs(t.lat - a.lat) < 0.0005 and abs(t.lon - a.lon) < 0.0007
    where a."number" is not null and a.street is not null)
  select kind, tid,
    arg_min("number" || ' ' || case when street = upper(street)
      then array_to_string(list_transform(string_split(lower(street), ' '), lambda w: upper(w[1]) || w[2:]), ' ') else street end, d) AS addr,
    arg_min(city, d) AS city, arg_min(country, d) AS country, min(d) AS ad
  from j where d <= 25 group by all;

create or replace table visit_attach as
  with j as (
    select v.id vid, p.id pid, dist_m(v.lat, v.lon, p.lat, p.lon) AS d
    from targets v join all_places p on abs(p.lat - v.lat) < 0.0005 and abs(p.lon - v.lon) < 0.0007 where v.kind = 'visit')
  select vid, arg_min(pid, d) AS pid, min(d) AS d from j where d <= 35 group by vid;

create or replace table decisions as
  select t.kind, t.id tid, b.cid, b.pname, b.category, b.score, b.d, na.addr, na.ad, na.city, na.country,
    case
      when t.kind = 'place' and s.photon_poi and coalesce(b.score, 0) < 0.7 then null
      when b.pname is not null and b.score >= (case when coalesce(s.is_google, false) then 0.35 else 0.45 end)
           and (coalesce(s.overnight, 0) < 0.3 or is_lodging(b.category)) then 'poi'
      when na.addr is not null then 'address'
    end how
  from targets t
  left join ptargets s on t.kind = 'place' and s.id = t.id
  left join best b on b.kind = t.kind and b.tid = t.id
  left join nearaddr na on na.kind = t.kind and na.tid = t.id;

copy (
  select 'place' AS kind, tid AS id, coalesce(how, 'none') AS how, case how when 'poi' then pname else addr end AS new_name,
    case how when 'poi' then cid end AS overture_id, case how when 'poi' then category end AS category,
    round(case how when 'poi' then d else ad end, 1) AS distance_m, city, country, null::bigint AS place_id
  from decisions where kind = 'place'
  union all
  select 'visit', v.vid, 'place', null, null, null, round(v.d, 1), null, null, v.pid from visit_attach v
  union all
  select 'visit', tid, how, case how when 'poi' then pname else addr end, case how when 'poi' then cid end, category,
    round(case how when 'poi' then d else ad end, 1), null, null, null
  from decisions where kind = 'visit' and how is not null and tid not in (select vid from visit_attach)
) to 'decisions.json' (format json, array true);
select how, count(*) from decisions group by all;
