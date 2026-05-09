local M = {}

function M.setup(ctx)
  local _ENV = ctx
  local function same_class_windows(class_name)
    local windows = {}
    if not class_name or class_name == "" then
      return windows
    end

    for _, window in ipairs(hl.get_windows()) do
      if is_normal_window(window) and window.class == class_name then
        windows[#windows + 1] = window
      end
    end

    return windows
  end

  local function short_text(value, limit)
    value = tostring(value or "")
    value = value:gsub("[%c\t\r\n]", " ")
    if #value <= limit then
      return value
    end
    return value:sub(1, limit - 3) .. "..."
  end

  local function normal_windows()
    local windows = {}
    for _, window in ipairs(hl.get_windows()) do
      if is_normal_window(window) then
        windows[#windows + 1] = window
      end
    end

    table.sort(windows, function(left, right)
      local left_workspace = left.workspace and left.workspace.id or max_workspace + 1
      local right_workspace = right.workspace and right.workspace.id or max_workspace + 1
      if left_workspace ~= right_workspace then
        return left_workspace < right_workspace
      end
      return (left.focus_history_id or 0) < (right.focus_history_id or 0)
    end)

    return windows
  end

  local function window_picker_entry(index, window)
    local workspace = window.workspace and window.workspace.id or "?"
    local class = short_text(window.class, 18)
    local title = short_text(window.title, 48)
    return tostring(index) .. "  [" .. tostring(workspace) .. "] " .. class .. "  " .. title
  end

  local function remove_minimized_window(target)
    local remaining = {}
    local target_address = target and target.address
    for _, window in ipairs(minimized_windows) do
      if window and window.address ~= target_address then
        remaining[#remaining + 1] = window
      end
    end
    minimized_windows = remaining
  end

  local function add_minimized_window(window)
    if not window or not window.address then
      return
    end

    remove_minimized_window(window)
    minimized_windows[#minimized_windows + 1] = window
  end

  local function hydrate_minimized_windows()
    local by_address = {}
    local current_by_address = {}
    local hydrated = {}

    for _, window in ipairs(hl.get_windows()) do
      if window and window.address then
        current_by_address[window.address] = window
      end
    end

    for _, window in ipairs(minimized_windows) do
      local current = window and window.address and current_by_address[window.address]
      if current and is_minimized_window(current) and not by_address[current.address] then
        by_address[current.address] = true
        hydrated[#hydrated + 1] = current
      end
    end

    for _, window in pairs(current_by_address) do
      if window and window.address and is_minimized_window(window) and not by_address[window.address] then
        by_address[window.address] = true
        hydrated[#hydrated + 1] = window
      end
    end

    minimized_windows = hydrated
  end

  local function window_workspace_name(window)
    return window and window.workspace and window.workspace.name or ""
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

  local function apply_scratchpad_geometry(name, window, target_monitor)
    local def = scratchpads[name]
    local monitor = target_monitor or hl.get_active_monitor()
    if not def or not window or not monitor then
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
    else
      width = math.floor(workarea.width * scratchpad_size_ratio)
      height = math.floor(workarea.height * scratchpad_size_ratio)
      x = workarea.x + math.floor((workarea.width - width) / 2)
      y = workarea.y + math.floor((workarea.height - height) / 2)
    end
    local selector = window_selector(window)

    dispatch(hl.dsp.window.float({ action = "enable", window = selector }))
    dispatch(hl.dsp.window.tag({ tag = "+scratchpad", window = selector }))
    dispatch(hl.dsp.window.tag({ tag = "+scratchpad-" .. name, window = selector }))
    dispatch(hl.dsp.window.resize({ x = width, y = height, relative = false, window = selector }))
    dispatch(hl.dsp.window.move({ x = x, y = y, relative = false, window = selector }))
    if def.dropdown then
      dispatch(hl.dsp.window.set_prop({ prop = "border_size", value = "0", window = selector }))
      dispatch(hl.dsp.window.set_prop({ prop = "no_shadow", value = "1", window = selector }))
    end
  end

  local function float_active_window_preserving_tiled_geometry()
    local geometry = tiled_window_geometry(hl.get_active_window())
    dispatch(hl.dsp.window.float({ action = "enable", window = geometry and geometry.selector or nil }))
    if geometry then
      dispatch(hl.dsp.window.resize({ x = geometry.width, y = geometry.height, relative = false, window = geometry.selector }))
      dispatch(hl.dsp.window.move({ x = geometry.x, y = geometry.y, relative = false, window = geometry.selector }))
    end
    return geometry
  end

  local function float_and_drag_active_window()
    float_active_window_preserving_tiled_geometry()
    dispatch(hl.dsp.window.drag())
  end

  local function float_and_resize_active_window()
    float_active_window_preserving_tiled_geometry()
    dispatch(hl.dsp.window.resize())
  end

  local function toggle_pinned_active_window()
    local window = hl.get_active_window()
    local selector = window_selector(window)
    if not window or not selector then
      return
    end

    if window.pinned then
      dispatch(hl.dsp.window.pin({ action = "disable", window = selector }))
      dispatch(hl.dsp.window.float({ action = "disable", window = selector }))
      return
    end

    if not window.floating then
      float_active_window_preserving_tiled_geometry()
    end
    dispatch(hl.dsp.window.pin({ action = "enable", window = selector }))
  end

  local function schedule_scratchpad_geometry(name, window, target_monitor)
    hl.timer(function()
      apply_scratchpad_geometry(name, window, target_monitor)
    end, { timeout = 50, type = "oneshot" })
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
    move_window_to_workspace(workspace.id, false, window)
    dispatch(hl.dsp.focus({ window = window_selector(window) }))
    schedule_scratchpad_geometry(name, window, target_monitor or hl.get_active_monitor())
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

  local function current_minimized_windows()
    hydrate_minimized_windows()

    local windows = {}
    for _, window in ipairs(minimized_windows) do
      if window and window.address and is_minimized_window(window) then
        windows[#windows + 1] = window
      end
    end
    minimized_windows = windows
    return windows
  end

  local function restore_minimized_window(window, workspace)
    if not window or not workspace then
      return false
    end

    move_window_to_workspace(workspace.id, false, window)
    return true
  end

  local function window_picker_candidates_for(mode)
    if mode == "minimized" then
      return current_minimized_windows()
    end

    local focused = hl.get_active_window()
    local workspace = active_workspace()
    local candidates = {}

    for _, window in ipairs(normal_windows()) do
      local include = true
      if mode == "bring" and workspace and window.workspace == workspace then
        include = false
      elseif mode == "replace" and focused and window == focused then
        include = false
      end

      if include then
        candidates[#candidates + 1] = window
      end
    end

    return candidates
  end

  local function activate_window_picker_candidate(index)
    local window = window_picker_candidates[index]
    local mode = window_picker_mode
    window_picker_mode = nil
    window_picker_candidates = {}
    dispatch(hl.dsp.submap("reset"))

    if not window then
      return
    end

    if mode == "go" then
      dispatch(hl.dsp.focus({ window = window_selector(window) }))
      return
    end

    local workspace = active_workspace()
    if mode == "bring" and workspace then
      move_window_to_workspace(workspace.id, false, window)
      dispatch(hl.dsp.focus({ window = window_selector(window) }))
      return
    end

    if mode == "minimized" and workspace then
      remove_minimized_window(window)
      restore_minimized_window(window, workspace)
      dispatch(hl.dsp.focus({ window = window_selector(window) }))
      return
    end

    if mode == "replace" then
      local focused = hl.get_active_window()
      if focused and focused ~= window then
        dispatch(hl.dsp.window.swap({ target = window_selector(window), window = window_selector(focused) }))
        dispatch(hl.dsp.focus({ window = window_selector(window) }))
      end
    end
  end

  local function enter_window_picker(mode)
    window_picker_mode = mode
    window_picker_candidates = window_picker_candidates_for(mode)

    if #window_picker_candidates == 0 then
      local empty_text = "No windows available"
      if mode == "minimized" then
        empty_text = "No minimized windows"
      end

      hl.notification.create({
        text = empty_text,
        duration = 1800,
        icon = notification_icons.info,
        color = "rgba(edb443ff)",
        font_size = 13,
      })
      return
    end

    local lines = {}
    local count = math.min(#window_picker_candidates, 9)
    for i = 1, count do
      lines[#lines + 1] = window_picker_entry(i, window_picker_candidates[i])
    end

    hl.notification.create({
      text = table.concat(lines, "\n"),
      duration = 5000,
      icon = notification_icons.info,
      color = "rgba(edb443ff)",
      font_size = 11,
    })
    dispatch(hl.dsp.submap("window-picker"))
  end

  local function gather_focused_class()
    local focused = hl.get_active_window()
    local workspace = active_workspace()
    if not focused or not workspace or not focused.class or focused.class == "" then
      return
    end

    local count = 0
    for _, window in ipairs(same_class_windows(focused.class)) do
      if window ~= focused and window.workspace ~= workspace then
        move_window_to_workspace(workspace.id, false, window)
        count = count + 1
      end
    end

    hl.notification.create({
      text = "Gathered " .. tostring(count) .. " " .. focused.class .. " windows",
      duration = 1600,
      icon = notification_icons.info,
      color = "rgba(edb443ff)",
      font_size = 13,
    })
  end

  local function focus_next_class()
    local focused = hl.get_active_window()
    if not focused or not focused.class or focused.class == "" then
      dispatch(hl.dsp.window.cycle_next({ next = true, tiled = true, floating = false }))
      return
    end

    local classes = {}
    local first_by_class = {}
    for _, window in ipairs(hl.get_windows()) do
      if is_normal_window(window) and window.class and window.class ~= "" and not first_by_class[window.class] then
        first_by_class[window.class] = window
        classes[#classes + 1] = window.class
      end
    end

    table.sort(classes)
    if #classes <= 1 then
      return
    end

    local current_index = 1
    for index, class_name in ipairs(classes) do
      if class_name == focused.class then
        current_index = index
        break
      end
    end

    local next_class = classes[(current_index % #classes) + 1]
    local target = first_by_class[next_class]
    if target then
      dispatch(hl.dsp.focus({ window = window_selector(target) }))
    end
  end

  local function show_active_window_info()
    local window = hl.get_active_window()
    if not window then
      hl.notification.create({
        text = "No active window",
        duration = 1800,
        icon = notification_icons.info,
        color = "rgba(edb443ff)",
        font_size = 13,
      })
      return
    end

    local workspace = window.workspace and (window.workspace.name or window.workspace.id) or "?"
    local lines = {
      "Class: " .. tostring(window.class or ""),
      "Title: " .. tostring(window.title or ""),
      "Workspace: " .. tostring(workspace),
      "Pinned: " .. tostring(window.pinned or false),
      "Address: " .. tostring(window.address or ""),
      "PID: " .. tostring(window.pid or ""),
    }

    hl.notification.create({
      text = table.concat(lines, "\n"),
      duration = 5000,
      icon = notification_icons.info,
      color = "rgba(edb443ff)",
      font_size = 11,
    })
  end

  local function raise_or_spawn(class_fragment, command)
    local fragment = string.lower(class_fragment)
    for _, window in ipairs(hl.get_windows()) do
      if is_normal_window(window) and window.class and string.find(string.lower(window.class), fragment, 1, true) then
        dispatch(hl.dsp.focus({ window = window_selector(window) }))
        return
      end
    end

    hl.exec_cmd(command)
  end

  local function minimize_active_window()
    local window = hl.get_active_window()
    if not window then
      return
    end

    add_minimized_window(window)
    move_window_to_workspace(minimized_workspace, false, window)
  end

  local function restore_last_minimized()
    local workspace = active_workspace()
    if not workspace then
      return
    end

    hydrate_minimized_windows()

    while #minimized_windows > 0 do
      local window = table.remove(minimized_windows)
      if window and window.address and is_minimized_window(window) then
        restore_minimized_window(window, workspace)
        dispatch(hl.dsp.focus({ window = window_selector(window) }))
        return
      end
    end
  end

  local function restore_all_minimized()
    local workspace = active_workspace()
    if not workspace then
      return
    end

    hydrate_minimized_windows()

    while #minimized_windows > 0 do
      restore_minimized_window(table.remove(minimized_windows), workspace)
    end
  end

  local function minimize_other_classes()
    local focused = hl.get_active_window()
    local workspace = active_workspace()
    if not focused or not workspace then
      return
    end

    for _, window in ipairs(tiled_windows(workspace)) do
      if window ~= focused and window.class ~= focused.class then
        add_minimized_window(window)
        move_window_to_workspace(minimized_workspace, false, window)
      end
    end
  end

  local function restore_focused_class()
    local focused = hl.get_active_window()
    local workspace = active_workspace()
    if not focused or not workspace or not focused.class then
      return
    end

    hydrate_minimized_windows()

    local remaining = {}
    for _, window in ipairs(minimized_windows) do
      if window and window.class == focused.class and is_minimized_window(window) then
        restore_minimized_window(window, workspace)
      else
        remaining[#remaining + 1] = window
      end
    end
    minimized_windows = remaining
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

  ctx.same_class_windows = same_class_windows
  ctx.short_text = short_text
  ctx.normal_windows = normal_windows
  ctx.window_picker_entry = window_picker_entry
  ctx.remove_minimized_window = remove_minimized_window
  ctx.add_minimized_window = add_minimized_window
  ctx.hydrate_minimized_windows = hydrate_minimized_windows
  ctx.window_workspace_name = window_workspace_name
  ctx.scratchpad_workspace = scratchpad_workspace
  ctx.as_number = as_number
  ctx.logical_monitor_dimension = logical_monitor_dimension
  ctx.split_tsv = split_tsv
  ctx.monitor_from_reserved_fields = monitor_from_reserved_fields
  ctx.monitor_from_reserved_lines = monitor_from_reserved_lines
  ctx.monitor_from_reserved_cache = monitor_from_reserved_cache
  ctx.refresh_monitor_reserved_cache = refresh_monitor_reserved_cache
  ctx.monitor_workarea = monitor_workarea
  ctx.matching_scratchpad_windows = matching_scratchpad_windows
  ctx.apply_scratchpad_geometry = apply_scratchpad_geometry
  ctx.float_active_window_preserving_tiled_geometry = float_active_window_preserving_tiled_geometry
  ctx.float_and_drag_active_window = float_and_drag_active_window
  ctx.float_and_resize_active_window = float_and_resize_active_window
  ctx.toggle_pinned_active_window = toggle_pinned_active_window
  ctx.schedule_scratchpad_geometry = schedule_scratchpad_geometry
  ctx.hide_scratchpad_window = hide_scratchpad_window
  ctx.show_scratchpad_window = show_scratchpad_window
  ctx.scratchpad_is_visible = scratchpad_is_visible
  ctx.active_scratchpad_windows = active_scratchpad_windows
  ctx.hide_active_scratchpads = hide_active_scratchpads
  ctx.refresh_active_scratchpad_geometries = refresh_active_scratchpad_geometries
  ctx.refresh_active_scratchpad_geometries_later = refresh_active_scratchpad_geometries_later
  ctx.refresh_shell_workarea_and_scratchpads = refresh_shell_workarea_and_scratchpads
  ctx.adopt_matching_scratchpad_window = adopt_matching_scratchpad_window
  ctx.current_minimized_windows = current_minimized_windows
  ctx.restore_minimized_window = restore_minimized_window
  ctx.window_picker_candidates_for = window_picker_candidates_for
  ctx.activate_window_picker_candidate = activate_window_picker_candidate
  ctx.enter_window_picker = enter_window_picker
  ctx.gather_focused_class = gather_focused_class
  ctx.focus_next_class = focus_next_class
  ctx.show_active_window_info = show_active_window_info
  ctx.raise_or_spawn = raise_or_spawn
  ctx.minimize_active_window = minimize_active_window
  ctx.restore_last_minimized = restore_last_minimized
  ctx.restore_all_minimized = restore_all_minimized
  ctx.minimize_other_classes = minimize_other_classes
  ctx.restore_focused_class = restore_focused_class
  ctx.toggle_scratchpad = toggle_scratchpad
end

return M
