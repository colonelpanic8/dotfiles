email=$1
share=$2
cd "$CACHE_DIRECTORY"
export HOME=$CACHE_DIRECTORY
rm -f work.db work.db.wal ./*.tsv decisions.json

release=$(curl -fsS 'https://overturemaps-us-west-2.s3.amazonaws.com/?list-type=2&prefix=release/&delimiter=/' |
  grep -o 'release/[0-9.-]*/' | sort | tail -n 1 | cut -d/ -f2)
[ -n "$release" ]
echo "Overture release $release"

psql -X -q -v ON_ERROR_STOP=1 -v email="$email" -v release="$release" -d dawarich -f "$share/targets.sql"
if [ ! -s places.tsv ] && [ ! -s visits.tsv ]; then
  echo "Nothing to match"
  exit 0
fi

cat places.tsv visits.tsv | cut -f2,3 > coords.tsv

places="s3://overturemaps-us-west-2/release/$release/theme=places/type=place/*"
addresses="s3://overturemaps-us-west-2/release/$release/theme=addresses/type=address/*"
{
  cat <<'SQL'
INSTALL httpfs; LOAD httpfs; SET s3_region='us-west-2';
create table t as select * from read_csv('coords.tsv', delim='\t', header=false, columns={'lat': 'DOUBLE', 'lon': 'DOUBLE'});
create table cand (id varchar, lat double, lon double, name varchar, category varchar, confidence double, operating_status varchar);
create table addr (id varchar, lat double, lon double, number varchar, street varchar, city varchar, country varchar);
SQL
  duckdb -noheader -list -c "select distinct floor(lat * 4) / 4, floor(lon * 4) / 4 from read_csv('coords.tsv', delim='\t', header=false, columns={'lat': 'DOUBLE', 'lon': 'DOUBLE'})" |
    while IFS='|' read -r lat lon; do
      cat <<SQL
insert into cand select * from (
  select id, (bbox.ymin + bbox.ymax) / 2 lat, (bbox.xmin + bbox.xmax) / 2 lon, names."primary", taxonomy."primary", confidence, operating_status
  from read_parquet('$places', hive_partitioning = 1)
  where bbox.xmin between $lon - 0.004 and $lon + 0.254 and bbox.ymin between $lat - 0.003 and $lat + 0.253) o
where exists (select 1 from t where abs(t.lat - o.lat) < 0.003 and abs(t.lon - o.lon) < 0.004);
insert into addr select * from (
  select id, bbox.ymin lat, bbox.xmin lon, "number", street, coalesce(postal_city, address_levels[length(address_levels)]."value"), country
  from read_parquet('$addresses', hive_partitioning = 1)
  where bbox.xmin between $lon - 0.002 and $lon + 0.252 and bbox.ymin between $lat - 0.002 and $lat + 0.252) o
where exists (select 1 from t where abs(t.lat - o.lat) < 0.0008 and abs(t.lon - o.lon) < 0.001);
SQL
    done
} > fetch.sql
duckdb work.db -f fetch.sql
duckdb work.db -f "$share/match.sql"

SECRET_KEY_BASE="$(systemd-creds cat SECRET_KEY_BASE)"
export SECRET_KEY_BASE
rails runner "$share/apply.rb" "$email" "$CACHE_DIRECTORY/decisions.json" "$release"
