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

  local function float_active_window_to_default_scratchpad_geometry()
    local window = hl.get_active_window()
    local selector = window_selector(window)
    if not selector then
      return
    end

    local geometry = default_scratchpad_geometry(hl.get_active_monitor())
    if not geometry then
      return
    end

    dispatch(hl.dsp.window.fullscreen_state({
      internal = 0,
      client = 0,
      action = "set",
      window = selector,
    }))
    mark_window_as_floating_scratchpad(window, matching_scratchpad_name(window))
    dispatch(hl.dsp.window.resize({ x = geometry.width, y = geometry.height, relative = false, window = selector }))
    dispatch(hl.dsp.window.move({ x = geometry.x, y = geometry.y, relative = false, window = selector }))
  end

  local function tile_or_float_active_window()
    local window = hl.get_active_window()
    local selector = window_selector(window)
    if not selector then
      return
    end

    if window.floating then
      clear_floating_scratchpad_state(window)
      dispatch(hl.dsp.window.float({ action = "disable", window = selector }))
      return
    end

    float_active_window_to_default_scratchpad_geometry()
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

  local function window_picker_candidates_for(mode)
    if mode == "minimized" then
      return minimized_windows()
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
    if (mode == "bring" or mode == "minimized") and workspace then
      move_window_to_workspace(workspace.id, false, window)
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

  local function window_has_tag(window, tag)
    for _, value in ipairs((window and window.tags) or {}) do
      if tostring(value):gsub("%*$", "") == tag then
        return true
      end
    end

    return false
  end

  local function value_matches_any_pattern(value, patterns)
    value = tostring(value or "")
    if value == "" then
      return false
    end

    for _, pattern in ipairs(patterns or {}) do
      if value:find(pattern) then
        return true
      end
    end
    return false
  end

  local function is_game_like_window(window)
    if not window then
      return false
    end

    if window_has_tag(window, gaming_window_disabled_tag) then
      return false
    end
    if
      value_matches_any_pattern(window.class, gaming_window_excluded_class_patterns)
      or value_matches_any_pattern(window.initial_class, gaming_window_excluded_class_patterns)
      or value_matches_any_pattern(window.title, gaming_window_excluded_title_patterns)
      or value_matches_any_pattern(window.initial_title, gaming_window_excluded_title_patterns)
    then
      return false
    end

    return tostring(window.content_type or window.content or "") == "game"
      or window_has_tag(window, gaming_window_tag)
      or value_matches_any_pattern(window.class, gaming_window_class_patterns)
      or value_matches_any_pattern(window.initial_class, gaming_window_class_patterns)
      or value_matches_any_pattern(window.title, gaming_window_title_patterns)
      or value_matches_any_pattern(window.initial_title, gaming_window_title_patterns)
  end

  local function set_window_gaming_mode(window, enabled, opts)
    local selector = window_selector(window)
    if not selector then
      return
    end

    if enabled then
      dispatch(hl.dsp.window.tag({ tag = "-" .. gaming_window_disabled_tag, window = selector }))
      dispatch(hl.dsp.window.tag({ tag = "+" .. gaming_window_tag, window = selector }))
      dispatch(hl.dsp.window.fullscreen_state({
        internal = 2,
        client = 2,
        action = "set",
        window = selector,
      }))
    else
      dispatch(hl.dsp.window.tag({ tag = "-" .. gaming_window_tag, window = selector }))
      dispatch(hl.dsp.window.tag({ tag = "+" .. gaming_window_disabled_tag, window = selector }))
      dispatch(hl.dsp.window.fullscreen_state({
        internal = 0,
        client = 0,
        action = "set",
        window = selector,
      }))
    end

    if not (opts and opts.quiet) then
      hl.notification.create({
        text = "Gaming fullscreen: " .. (enabled and "on" or "off"),
        duration = 1600,
        icon = enabled and notification_icons.ok or notification_icons.info,
        color = enabled and "rgba(33ccffee)" or "rgba(edb443ff)",
        font_size = 13,
      })
    end
  end

  local function toggle_active_window_gaming_mode()
    local window = hl.get_active_window()
    if not window then
      return
    end

    set_window_gaming_mode(window, not window_has_tag(window, gaming_window_tag))
  end

  local function toggle_active_window_real_fullscreen()
    local window = hl.get_active_window()
    if not window then
      return
    end

    local fullscreen = tonumber(window.fullscreen) or 0
    local fullscreen_client = tonumber(window.fullscreen_client) or 0
    local enabling = fullscreen ~= 2 or fullscreen_client ~= 2
    dispatch(hl.dsp.window.fullscreen_state({
      internal = enabling and 2 or 0,
      client = enabling and 2 or 0,
      action = "set",
      window = window_selector(window),
    }))
  end

  local function toggle_inactive_opacity_for_active_window()
    local window = hl.get_active_window()
    local selector = window_selector(window)
    if not selector then
      return
    end

    local disabling_reduction = not window_has_tag(window, inactive_opacity_override_tag)
    dispatch(hl.dsp.window.tag({ tag = inactive_opacity_override_tag, window = selector }))
    hl.notification.create({
      text = "Inactive opacity reduction: " .. (disabling_reduction and "off for window" or "on for window"),
      duration = 1600,
      icon = notification_icons.info,
      color = "rgba(edb443ff)",
      font_size = 13,
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

  ctx.same_class_windows = same_class_windows
  ctx.short_text = short_text
  ctx.normal_windows = normal_windows
  ctx.window_picker_entry = window_picker_entry
  ctx.float_active_window_preserving_tiled_geometry = float_active_window_preserving_tiled_geometry
  ctx.float_and_drag_active_window = float_and_drag_active_window
  ctx.float_and_resize_active_window = float_and_resize_active_window
  ctx.float_active_window_to_default_scratchpad_geometry = float_active_window_to_default_scratchpad_geometry
  ctx.tile_or_float_active_window = tile_or_float_active_window
  ctx.toggle_pinned_active_window = toggle_pinned_active_window
  ctx.window_picker_candidates_for = window_picker_candidates_for
  ctx.activate_window_picker_candidate = activate_window_picker_candidate
  ctx.enter_window_picker = enter_window_picker
  ctx.gather_focused_class = gather_focused_class
  ctx.focus_next_class = focus_next_class
  ctx.show_active_window_info = show_active_window_info
  ctx.window_has_tag = window_has_tag
  ctx.value_matches_any_pattern = value_matches_any_pattern
  ctx.is_game_like_window = is_game_like_window
  ctx.set_window_gaming_mode = set_window_gaming_mode
  ctx.toggle_active_window_gaming_mode = toggle_active_window_gaming_mode
  ctx.toggle_active_window_real_fullscreen = toggle_active_window_real_fullscreen
  ctx.toggle_inactive_opacity_for_active_window = toggle_inactive_opacity_for_active_window
  ctx.raise_or_spawn = raise_or_spawn
end

return M
