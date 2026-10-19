#!/bin/sh
# Writes index.html, fleet.age, and settings.age next to this script for GitHub Pages.
# Requires qrencode and jq.
set -eu

BASE_URL=https://colonelpanic8.github.io/dotfiles/t3code/
FDROID_URL=https://colonelpanic8.github.io/t3code/fdroid/repo
RELEASES_URL=https://github.com/colonelpanic8/t3code/releases
APP_SCHEME=t3code-assembly
APP_PACKAGE=com.t3tools.t3code.assembly

here=$(cd "$(dirname "$0")" && pwd)
enrollment="$here/../../nixos/secrets/t3code-fleet-enrollment.age"

uri() { jq -rn --arg v "$1" '$v|@uri'; }

fleet_url="${BASE_URL}fleet.age"
query="fleetEnrollment=$(uri "$fleet_url")"
page_url="${BASE_URL}?${query}"
deep_link="${APP_SCHEME}://connections/new?${query}"
intent_url="intent://connections/new?${query}#Intent;scheme=${APP_SCHEME};package=${APP_PACKAGE};S.browser_fallback_url=$(uri "$FDROID_URL");end"

cp "$enrollment" "$here/fleet.age"
cp "$here/../../nixos/secrets/t3code-mobile-settings.age" "$here/settings.age"
settings_url="${BASE_URL}settings.age"

qr_svg=$(qrencode -t SVG --inline --svg-path -l M -m 4 "$page_url")

cat >"$here/index.html" <<EOF
<!doctype html>
<html lang="en">
<head>
<meta charset="utf-8">
<meta name="viewport" content="width=device-width, initial-scale=1">
<meta name="color-scheme" content="light dark">
<title>Join the T3 Code fleet</title>
<style>
body{font-family:system-ui,sans-serif;max-width:28rem;margin:0 auto;padding:1.5rem;line-height:1.5}
.qr{background:#fff;padding:1rem;border-radius:.5rem}
.qr svg{display:block;width:100%;height:auto;aspect-ratio:1;shape-rendering:crispEdges}
.open{display:block;margin:1.5rem 0 .5rem;padding:1rem;border-radius:.5rem;background:#1a73e8;color:#fff;text-align:center;font-size:1.15rem;font-weight:600;text-decoration:none}
small{opacity:.8}
</style>
</head>
<body>
<h1>Join the T3 Code fleet</h1>
<p>Scan this with your phone's camera. Already on the phone? Tap the button.</p>
<div class="qr">
$qr_svg
</div>
<a class="open" href="$intent_url">Open in T3 Code</a>
<p><small>Not on Chrome? <a href="$deep_link">Open with the t3code-assembly link</a>.</small></p>
<p>Requires the fleet enrollment key on the phone (Settings &rarr; fleet enrollment key).</p>
<h2>Install T3 Code</h2>
<ul>
<li><a href="$FDROID_URL">F-Droid repo</a> (also works in Obtainium)</li>
<li><a href="$RELEASES_URL">GitHub releases</a></li>
</ul>
<p><small>Encrypted enrollment: <a href="$fleet_url">fleet.age</a></small></p>
<p><small>Settings feed (Settings &rarr; Environments &rarr; Settings feed): <a href="$settings_url">$settings_url</a></small></p>
</body>
</html>
EOF
