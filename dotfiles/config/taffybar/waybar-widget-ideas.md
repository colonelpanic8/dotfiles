# Waybar Widget Ideas for Taffybar

Cross-reference of waybar modules vs taffybar's existing widgets, identifying gaps and opportunities.

## Widgets Taffybar Already Has (Equivalent Exists)

| Waybar Module | Taffybar Equivalent |
|---|---|
| battery / upower | Battery (UPower DBus) |
| cpu / cpu_usage | CPUMonitor, Text.CPUMonitor |
| memory | Text.MemoryMonitor |
| network | NetworkGraph, Text.NetworkMonitor, NetworkManager |
| disk | FSMonitor, DiskIOMonitor |
| clock / simpleclock | SimpleClock |
| pulseaudio | PulseAudio |
| mpris / mpd | MPRIS2 |
| tray (SNI) | SNITray |
| workspaces (sway/hyprland/etc) | Workspaces, HyprlandWorkspaces |
| window (sway/hyprland/etc) | Windows, HyprlandWindows |
| layout (sway/hyprland/etc) | Layout, HyprlandLayout |
| backlight | Backlight |
| custom script | CommandRunner, SimpleCommandButton |

## Widgets Taffybar Does NOT Have

### Tier 1: High Value / Commonly Useful

#### 1. Bluetooth Status
- **Waybar module:** `bluetooth`
- **What it does:** Shows Bluetooth controller status, connected devices, device battery levels
- **Data source:** BlueZ DBus interface (`org.bluez`)
- **Why useful:** Very common need on laptops. Shows headphone battery %, connection state. Currently no way to see BT status in taffybar without an external script via CommandRunner.

#### 2. Keyboard State (Caps Lock / Num Lock / Scroll Lock)
- **Waybar module:** `keyboard_state`
- **What it does:** Shows current state of keyboard lock keys
- **Data source:** libevdev / libinput keyboard devices
- **Why useful:** Prevents accidental caps-lock typing. Subtle but saves daily annoyance, especially for people who touch-type. Small, low-effort widget.

#### 3. System Temperature
- **Waybar module:** `temperature`
- **What it does:** Shows CPU/GPU/sensor temperatures with warning/critical thresholds
- **Data source:** `/sys/class/thermal/`, `/sys/class/hwmon/`, or lm_sensors
- **Why useful:** Essential for monitoring thermals on laptops, useful for desktop users too. Can warn before thermal throttling. Currently requires CommandRunner workaround.

#### 4. Idle / Sleep Inhibitor
- **Waybar module:** `idle_inhibitor`, `inhibitor`
- **What it does:** Toggle button to prevent screen lock / sleep. Waybar has both a Wayland protocol version (`idle_inhibitor` via `zwp_idle_inhibitor_v1`) and a systemd-based version (`inhibitor` via `org.freedesktop.login1`).
- **Data source:** Wayland idle inhibit protocol or systemd-logind DBus
- **Why useful:** Common need when watching videos, presenting, or running long tasks. One-click toggle in the bar is much nicer than running `systemd-inhibit` or `caffeine` manually.

#### 5. Privacy Indicator (Screen Share / Mic Recording)
- **Waybar module:** `privacy`
- **What it does:** Shows when microphone is being recorded or screen is being shared
- **Data source:** PipeWire node monitoring
- **Why useful:** Security/privacy awareness. Shows a visible indicator when apps are using mic or screenshare. Increasingly important with video conferencing being ubiquitous.

#### 6. WirePlumber / PipeWire Audio
- **Waybar module:** `wireplumber`
- **What it does:** Volume control using WirePlumber (PipeWire session manager) instead of PulseAudio
- **Data source:** WirePlumber API
- **Why useful:** PipeWire is replacing PulseAudio on modern Linux. While taffybar's PulseAudio widget works via pipewire-pulse compatibility, a native PipeWire widget would be more future-proof and could expose PipeWire-specific features.

#### 7. Power Profiles Daemon
- **Waybar module:** `power_profiles_daemon`
- **What it does:** Shows and toggles power profile (performance / balanced / power-saver)
- **Data source:** `net.hadess.PowerProfiles` DBus service
- **Why useful:** Laptop users frequently switch profiles. One-click toggle between performance and battery-saving modes is very handy.

#### 8. Systemd Failed Units
- **Waybar module:** `systemd_failed_units`
- **What it does:** Shows count of failed systemd services (both system and user)
- **Data source:** systemd DBus interface (`org.freedesktop.systemd1`)
- **Why useful:** Silent service failures are easy to miss. A small counter in the bar catches problems early. Especially useful for NixOS users who frequently rebuild.

#### 9. CPU Frequency
- **Waybar module:** `cpu_frequency`
- **What it does:** Shows current CPU frequency (min/max/average across cores)
- **Data source:** `/sys/devices/system/cpu/cpu*/cpufreq/scaling_cur_freq`
- **Why useful:** Useful for verifying power profiles are working, checking if CPU is throttling. Complements the existing CPU usage monitor.

#### 10. Load Average
- **Waybar module:** `load`
- **What it does:** Shows 1/5/15 minute load averages
- **Data source:** `/proc/loadavg` or `getloadavg()`
- **Why useful:** Classic Unix metric. More meaningful than instantaneous CPU % for understanding system pressure over time.

#### 11. User / Uptime
- **Waybar module:** `user`
- **What it does:** Shows logged-in user name and system uptime
- **Data source:** `/etc/passwd`, system uptime
- **Why useful:** Mildly useful for multi-user systems or just for knowing how long since last reboot. Low priority but trivial to implement.

#### 12. Hyprland Submap
- **Waybar module:** `hyprland/submap`
- **What it does:** Shows current Hyprland keybinding submap (like vim modes)
- **Data source:** Hyprland IPC socket events
- **Why useful:** If you use Hyprland submaps for modal keybinding (e.g. resize mode, move mode), this shows which mode you're in. Prevents confusion about why keys aren't doing what you expect.

#### 13. Hyprland Window Count
- **Waybar module:** `hyprland/windowcount`
- **What it does:** Shows number of windows in current workspace
- **Data source:** Hyprland IPC socket
- **Why useful:** Quick awareness of workspace density. Minor convenience.

#### 14. Image Widget
- **Waybar module:** `image`
- **What it does:** Displays a static or dynamically-updated image in the bar
- **Data source:** Image files, or external commands that return image paths
- **Why useful:** Generic building block. Could show avatar, logos, dynamic status images. Taffybar already has `Generic.Icon` and `Generic.AutoSizeImage` which partially cover this.

#### 15. Audio Visualizer (Cava)
- **Waybar module:** `cava`
- **What it does:** Real-time audio waveform/spectrum visualization in the bar
- **Data source:** PulseAudio/ALSA audio stream via cava
- **Why useful:** Pure eye candy. Fun but not functional. Could be interesting as a "is audio playing?" indicator.

### Tier 3: Niche / Low Priority

#### 16. GPS / GNSS
- **Waybar module:** `gps`
- **What it does:** Shows GPS fix status and coordinates
- **Data source:** gpsd socket
- **Why useful:** Very niche. Only relevant for mobile Linux devices or specialized setups.

#### 17. GameMode
- **Waybar module:** `gamemode`
- **What it does:** Shows if Feral GameMode is active and how many games are using it
- **Data source:** `com.feralinteractive.GameMode` DBus
- **Why useful:** Only relevant if you game on Linux with GameMode enabled. Niche.

#### 18. JACK Audio
- **Waybar module:** `jack`
- **What it does:** Shows JACK server status (sample rate, buffer size, xruns, CPU load)
- **Data source:** JACK client API
- **Why useful:** Only for audio production setups using JACK. Very niche.

#### 19. Sndio Audio
- **Waybar module:** `sndio`
- **What it does:** Volume control for sndio audio backend
- **Data source:** Sndio socket
- **Why useful:** Sndio is primarily a BSD audio system. Extremely niche on Linux.

#### 20. WLR Taskbar (Foreign Toplevel)
- **Waybar module:** `wlr/taskbar`
- **What it does:** Windows-style taskbar showing all open windows as buttons with icons
- **Data source:** `wlr-foreign-toplevel-management` Wayland protocol
- **Why useful:** Different UX paradigm from workspace-based switching. Some users prefer a traditional taskbar. Would be a significant undertaking.

#### 21. Scratchpad (Sway-specific)
- **Waybar module:** `sway/scratchpad`
- **What it does:** Shows count of windows in Sway's scratchpad
- **Why useful:** Sway-specific, not relevant for Hyprland.

#### 22. Slider Widgets (Backlight Slider, PulseAudio Slider)
- **Waybar modules:** `backlight_slider`, `pulseaudio_slider`
- **What it does:** Interactive slider controls (not just labels) for brightness/volume
- **Why useful:** Taffybar already supports scroll-wheel on PulseAudio and Backlight widgets, but dedicated slider UI could be nicer. Moderate effort for marginal improvement.

#### 23. C FFI Plugin System
- **Waybar module:** `cffi`
- **What it does:** Load custom C modules as waybar extensions
- **Why useful:** Taffybar already has a superior extensibility story via Haskell. Not needed.

## Recommendations Summary

**Start here (high impact, reasonable effort):**
1. **Bluetooth Status** - Very commonly needed, clean DBus API
2. **Keyboard State** - Small, simple, daily-useful
3. **Temperature** - Essential laptop monitoring
4. **Idle Inhibitor** - Common need, well-defined protocol
5. **Privacy Indicator** - Timely and security-relevant

**Second wave (useful but lower urgency):**
6. **Power Profiles Daemon** - Great for laptop users
7. **Systemd Failed Units** - Catches silent failures
8. **Hyprland Submap** - Quick win if you use submaps
9. **CPU Frequency** - Complements existing CPU monitor

**Skip unless specifically needed:**
- GPS, GameMode, JACK, Sndio, Cava, C FFI - too niche
- WLR Taskbar - different paradigm, huge effort
- Sliders - taffybar already has scroll-wheel control
