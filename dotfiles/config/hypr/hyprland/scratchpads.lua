local M = {}

function M.setup(ctx)
  local _ENV = ctx

  scratchpad_size_ratio = 0.95
  dropdown_height_ratio = 0.5
  dropdown_animation_frames = 18
  dropdown_animation_frame_ms = 16
  scratchpad_pending = {}
  monitor_reserved_cache_path = (os.getenv("XDG_RUNTIME_DIR") or "/tmp") .. "/hyprland-monitor-reserved.tsv"
  scratchpad_fallback_reserved_top = 60

  scratchpads = {
    codex = {
      command = "codex-desktop",
      class = "codex-desktop",
    },
    htop = {
      command = "alacritty --class htop-scratch --title htop -e htop",
      class = "htop-scratch",
    },
    volume = {
      command = "pavucontrol",
      class = "org.pulseaudio.pavucontrol",
    },
    spotify = {
      command = "spotify",
      class = "spotify",
    },
    element = {
      command = "element-desktop",
      classes = { "Element", "electron" },
      title = "Element",
    },
    slack = {
      command = "slack",
      class = "Slack",
    },
    messages = {
      command = "google-chrome-stable --profile-directory=Default --app=https://messages.google.com/web/conversations",
      class = "chrome-messages.google.com",
    },
    transmission = {
      command = "transmission-gtk",
      class = "transmission-gtk",
    },
    dropdown = {
      command = "ghostty --config-file=/home/imalison/.config/ghostty/dropdown",
      class = "com.mitchellh.ghostty.dropdown",
      dropdown = true,
    },
  }

  local function lower_contains(value, needle)
    if not needle or needle == "" then
      return true
    end

    value = string.lower(tostring(value or ""))
    needle = string.lower(tostring(needle))
    return value:find(needle, 1, true) ~= nil
  end

  local function lower_contains_any(value, needles)
    if type(needles) ~= "table" then
      return lower_contains(value, needles)
    end

    for _, needle in ipairs(needles) do
      if lower_contains(value, needle) then
        return true
      end
    end
    return false
  end

  local function scratchpad_window_matches(window, def)
    return window
      and not (type(is_file_chooser_window) == "function" and is_file_chooser_window(window))
      and lower_contains_any(window.class, def.classes or def.class)
      and lower_contains(window.title, def.title)
  end

  local function is_scratchpad_window(window)
    for _, def in pairs(scratchpads) do
      if scratchpad_window_matches(window, def) then
        return true
      end
    end
    return false
  end

  local function matching_scratchpad_name(window)
    for name, def in pairs(scratchpads) do
      if scratchpad_window_matches(window, def) then
        return name
      end
    end
    return nil
  end

  local function scratchpad_workspace(name)
    return "special:scratch-" .. name
  end

  local function as_number(value, default)
    local number = tonumber(value)
    if number == nil then
      return default
    end
    return number
  end

  local function logical_monitor_dimension(value, scale)
    value = as_number(value, 0)
    scale = as_number(scale, 1)
    if scale <= 0 then
      scale = 1
    end
    return math.floor((value / scale) + 0.5)
  end

  local function split_tsv(line)
    local fields = {}
    for field in (line .. "\t"):gmatch("([^\t]*)\t") do
      fields[#fields + 1] = field
    end
    return fields
  end

  local function monitor_from_reserved_fields(monitor, fields)
    if not monitor or not monitor.name or fields[1] ~= monitor.name or #fields < 10 then
      return nil
    end

    return {
      name = monitor.name,
      x = tonumber(fields[2]),
      y = tonumber(fields[3]),
      width = tonumber(fields[4]),
      height = tonumber(fields[5]),
      scale = tonumber(fields[6]),
      reserved = {
        tonumber(fields[7]),
        tonumber(fields[8]),
        tonumber(fields[9]),
        tonumber(fields[10]),
      },
    }
  end

  local function monitor_from_reserved_lines(monitor, lines)
    if not monitor or not monitor.name then
      return nil
    end

    for line in lines do
      local cached = monitor_from_reserved_fields(monitor, split_tsv(line))
      if cached then
        return cached
      end
    end
    return nil
  end

  local function monitor_from_reserved_cache(monitor)
    if verify_config or not monitor or not monitor.name then
      return nil
    end

    local file = io.open(monitor_reserved_cache_path, "r")
    if not file then
      return nil
    end

    local cached = monitor_from_reserved_lines(monitor, file:lines())
    file:close()
    return cached
  end

  local function refresh_monitor_reserved_cache(delay)
    if verify_config then
      return
    end

    local command = string.format(
      [=[sleep %.2f; cache="${XDG_RUNTIME_DIR:-/tmp}/hyprland-monitor-reserved.tsv"; tmp="$cache.tmp"; /run/current-system/sw/bin/hyprctl -j monitors 2>/dev/null | /run/current-system/sw/bin/jq -r '.[] | [.name, .x, .y, .width, .height, .scale, .reserved[0], .reserved[1], .reserved[2], .reserved[3]] | @tsv' > "$tmp" && mv "$tmp" "$cache"]=],
      as_number(delay, 0)
    )
    hl.exec_cmd("sh -lc " .. shell_quote(command))
  end

  local function monitor_workarea(monitor)
    monitor = monitor_from_reserved_cache(monitor) or monitor
    local width = logical_monitor_dimension(monitor.width, monitor.scale)
    local height = logical_monitor_dimension(monitor.height, monitor.scale)
    local reserved = monitor.reserved or { 0, scratchpad_fallback_reserved_top, 0, 0 }
    local left = math.floor(as_number(reserved[1], 0))
    local top = math.floor(as_number(reserved[2], 0))
    local right = math.floor(as_number(reserved[3], 0))
    local bottom = math.floor(as_number(reserved[4], 0))
    local work_width = width - left - right
    local work_height = height - top - bottom

    if work_width <= 0 then
      left = 0
      right = 0
      work_width = width
    end
    if work_height <= 0 then
      top = 0
      bottom = 0
      work_height = height
    end

    return {
      x = math.floor(as_number(monitor.x, 0)) + left,
      y = math.floor(as_number(monitor.y, 0)) + top,
      width = work_width,
      height = work_height,
    }
  end

  local function matching_scratchpad_windows(name)
    local def = scratchpads[name]
    local windows = {}
    if not def then
      return windows
    end

    for _, window in ipairs(hl.get_windows()) do
      if scratchpad_window_matches(window, def) then
        windows[#windows + 1] = window
      end
    end

    return windows
  end

  local function scratchpad_geometry(name, target_monitor, position)
    local def = scratchpads[name]
    local monitor = target_monitor or hl.get_active_monitor()
    if not def or not monitor then
      return
    end

    local workarea = monitor_workarea(monitor)
    local width
    local height
    local x
    local y
    if def.dropdown then
      width = workarea.width
      height = math.floor(workarea.height * dropdown_height_ratio)
      x = workarea.x
      y = workarea.y
      if position == "above" then
        y = workarea.y - height
      elseif type(position) == "number" then
        y = position
      end
    else
      width = math.floor(workarea.width * scratchpad_size_ratio)
      height = math.floor(workarea.height * scratchpad_size_ratio)
      x = workarea.x + math.floor((workarea.width - width) / 2)
      y = workarea.y + math.floor((workarea.height - height) / 2)
    end

    return {
      width = width,
      height = height,
      x = x,
      y = y,
    }
  end

  local function apply_scratchpad_geometry(name, window, target_monitor, position)
    local def = scratchpads[name]
    if not def or not window then
      return
    end

    local geometry = scratchpad_geometry(name, target_monitor, position)
    if not geometry then
      return
    end
    local selector = window_selector(window)

    dispatch(hl.dsp.window.float({ action = "enable", window = selector }))
    dispatch(hl.dsp.window.tag({ tag = "+scratchpad", window = selector }))
    dispatch(hl.dsp.window.tag({ tag = "+scratchpad-" .. name, window = selector }))
    dispatch(hl.dsp.window.resize({ x = geometry.width, y = geometry.height, relative = false, window = selector }))
    dispatch(hl.dsp.window.move({ x = geometry.x, y = geometry.y, relative = false, window = selector }))
    if def.dropdown then
      dispatch(hl.dsp.window.set_prop({ prop = "border_size", value = "0", window = selector }))
      dispatch(hl.dsp.window.set_prop({ prop = "no_shadow", value = "1", window = selector }))
    end
  end

  local function schedule_scratchpad_geometry(name, window, target_monitor, position, timeout)
    hl.timer(function()
      apply_scratchpad_geometry(name, window, target_monitor, position)
    end, { timeout = timeout or 50, type = "oneshot" })
  end

  local function dropdown_spring_progress(progress)
    if progress >= 1 then
      return 1
    end
    return 1 - (math.exp(-5.0 * progress) * math.cos(7.0 * progress))
  end

  local function animate_dropdown_scratchpad_down(name, window, target_monitor)
    local from = scratchpad_geometry(name, target_monitor, "above")
    local to = scratchpad_geometry(name, target_monitor)
    if not from or not to then
      schedule_scratchpad_geometry(name, window, target_monitor, nil, 35)
      return
    end

    for frame = 1, dropdown_animation_frames do
      local progress = frame / dropdown_animation_frames
      local eased = dropdown_spring_progress(progress)
      local y = math.floor(from.y + ((to.y - from.y) * eased) + 0.5)
      schedule_scratchpad_geometry(name, window, target_monitor, y, frame * dropdown_animation_frame_ms)
    end
  end

  local function hide_scratchpad_window(name, window)
    remove_minimized_window(window)
    move_window_to_workspace(scratchpad_workspace(name), false, window)
  end

  local function show_scratchpad_window(name, window, workspace, target_monitor)
    workspace = workspace or active_workspace()
    if not workspace then
      return
    end

    remove_minimized_window(window)
    if scratchpads[name] and scratchpads[name].dropdown then
      apply_scratchpad_geometry(name, window, target_monitor or hl.get_active_monitor(), "above")
    end
    move_window_to_workspace(workspace.id, false, window)
    dispatch(hl.dsp.focus({ window = window_selector(window) }))
    if scratchpads[name] and scratchpads[name].dropdown then
      animate_dropdown_scratchpad_down(name, window, target_monitor or hl.get_active_monitor())
    else
      schedule_scratchpad_geometry(name, window, target_monitor or hl.get_active_monitor())
    end
  end

  local function scratchpad_is_visible(window)
    local workspace = active_workspace()
    return workspace and window and same_workspace(window.workspace, workspace)
  end

  -- Active scratchpads are scratchpad windows visible on the active workspace.
  -- Invoking a different scratchpad replaces that active set.
  local function active_scratchpad_windows(except_name)
    local windows = {}
    for _, window in ipairs(hl.get_windows()) do
      local name = matching_scratchpad_name(window)
      if name and name ~= except_name and scratchpad_is_visible(window) then
        windows[#windows + 1] = {
          name = name,
          window = window,
        }
      end
    end
    return windows
  end

  local function hide_active_scratchpads(except_name)
    for _, active in ipairs(active_scratchpad_windows(except_name)) do
      hide_scratchpad_window(active.name, active.window)
    end
  end

  local function refresh_active_scratchpad_geometries()
    local monitor = hl.get_active_monitor()
    for _, active in ipairs(active_scratchpad_windows()) do
      schedule_scratchpad_geometry(active.name, active.window, monitor)
    end
  end

  local function refresh_active_scratchpad_geometries_later(timeout)
    hl.timer(refresh_active_scratchpad_geometries, { timeout = timeout or 300, type = "oneshot" })
  end

  local function refresh_shell_workarea_and_scratchpads()
    refresh_monitor_reserved_cache(0.15)
    refresh_active_scratchpad_geometries_later(400)
  end

  local function adopt_matching_scratchpad_window(window)
    if not window then
      return
    end

    for name, def in pairs(scratchpads) do
      if scratchpad_window_matches(window, def) then
        if scratchpad_pending[name] then
          local pending = scratchpad_pending[name]
          scratchpad_pending[name] = nil
          show_scratchpad_window(name, window, pending.workspace or active_workspace(), pending.monitor or hl.get_active_monitor())
        elseif scratchpad_is_visible(window) then
          schedule_scratchpad_geometry(name, window, hl.get_active_monitor())
        end
      end
    end
  end

  local function toggle_scratchpad(name)
    local def = scratchpads[name]
    if not def then
      return
    end

    if current_layout == monocle_layout then
      set_layout(columns_layout)
    end

    local windows = matching_scratchpad_windows(name)
    if #windows == 0 then
      hide_active_scratchpads(name)
      scratchpad_pending[name] = {
        monitor = hl.get_active_monitor(),
        workspace = active_workspace(),
      }
      hl.exec_cmd(def.command)
      return
    end

    local any_visible = false
    for _, window in ipairs(windows) do
      if scratchpad_is_visible(window) then
        any_visible = true
        break
      end
    end

    if any_visible then
      for _, window in ipairs(windows) do
        hide_scratchpad_window(name, window)
      end
    else
      hide_active_scratchpads(name)
      local workspace = active_workspace()
      local target_monitor = hl.get_active_monitor()
      for _, window in ipairs(windows) do
        show_scratchpad_window(name, window, workspace, target_monitor)
      end
    end
  end

  ctx.lower_contains = lower_contains
  ctx.lower_contains_any = lower_contains_any
  ctx.scratchpad_window_matches = scratchpad_window_matches
  ctx.is_scratchpad_window = is_scratchpad_window
  ctx.matching_scratchpad_name = matching_scratchpad_name
  ctx.scratchpad_workspace = scratchpad_workspace
  ctx.as_number = as_number
  ctx.logical_monitor_dimension = logical_monitor_dimension
  ctx.split_tsv = split_tsv
  ctx.monitor_from_reserved_fields = monitor_from_reserved_fields
  ctx.monitor_from_reserved_lines = monitor_from_reserved_lines
  ctx.monitor_from_reserved_cache = monitor_from_reserved_cache
  ctx.refresh_monitor_reserved_cache = refresh_monitor_reserved_cache
  ctx.monitor_workarea = monitor_workarea
  ctx.scratchpad_geometry = scratchpad_geometry
  ctx.matching_scratchpad_windows = matching_scratchpad_windows
  ctx.apply_scratchpad_geometry = apply_scratchpad_geometry
  ctx.schedule_scratchpad_geometry = schedule_scratchpad_geometry
  ctx.dropdown_spring_progress = dropdown_spring_progress
  ctx.animate_dropdown_scratchpad_down = animate_dropdown_scratchpad_down
  ctx.hide_scratchpad_window = hide_scratchpad_window
  ctx.show_scratchpad_window = show_scratchpad_window
  ctx.scratchpad_is_visible = scratchpad_is_visible
  ctx.active_scratchpad_windows = active_scratchpad_windows
  ctx.hide_active_scratchpads = hide_active_scratchpads
  ctx.refresh_active_scratchpad_geometries = refresh_active_scratchpad_geometries
  ctx.refresh_active_scratchpad_geometries_later = refresh_active_scratchpad_geometries_later
  ctx.refresh_shell_workarea_and_scratchpads = refresh_shell_workarea_and_scratchpads
  ctx.adopt_matching_scratchpad_window = adopt_matching_scratchpad_window
  ctx.toggle_scratchpad = toggle_scratchpad
end

return M
