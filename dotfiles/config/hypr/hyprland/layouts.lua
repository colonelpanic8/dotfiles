local M = {}

function M.setup(ctx)
  local _ENV = ctx
  local configure_quadrants_master
  local focus_workspace
  local move_window_to_workspace

  local function is_nstack_layout(layout)
    return layout == columns_layout or layout == grid_layout
  end

  local function hyprland_layout(layout)
    if layout == quadrants_layout then
      return large_main_layout
    elseif layout == grid_layout then
      return columns_layout
    end
    return layout
  end

  configure_quadrants_master = function()
    if quadrants_arranging or current_layout ~= quadrants_layout then
      return
    end

    local workspace = active_workspace()
    if not is_normal_workspace(workspace) then
      return
    end

    local windows = tiled_windows(workspace)
    if #windows == 0 then
      return
    end

    sort_windows_by_visual_position(windows)

    quadrants_arranging = true
    dispatch(hl.dsp.focus({ window = window_selector(windows[1]) }))
    dispatch(hl.dsp.layout("orientationleft"))
    dispatch(hl.dsp.layout("mfact exact 0.5"))

    for _ = 1, #windows do
      dispatch(hl.dsp.layout("removemaster"))
    end

    if #windows >= 3 then
      dispatch(hl.dsp.layout("addmaster"))
    end

    quadrants_arranging = false
    focus_workspace(workspace.id)
  end

  local function update_nstack_count()
    if current_layout == quadrants_layout then
      configure_quadrants_master()
      return
    end

    if not enable_nstack or not is_nstack_layout(current_layout) then
      return
    end

    local workspace = hl.get_active_workspace()
    local count = tiled_window_count(workspace)
    if count == 0 then
      return
    end

    if current_layout == columns_layout and nstack_supports_auto_stacks() then
      -- The plugin sizes stacks to the window count on every recalculation,
      -- which also covers paths that fire no window events (e.g. float -> tile).
      dispatch(hl.dsp.layout("setstackcount auto"))
      return
    end

    local stack_count = count
    if current_layout == grid_layout then
      stack_count = math.ceil(math.sqrt(count))
    end

    stack_count = math.max(stack_count, 2)
    dispatch(hl.dsp.layout("setstackcount " .. tostring(stack_count)))
  end

  local function schedule_nstack_count_update()
    if stack_update_timer then
      stack_update_timer:set_timeout(25)
      return
    end
    stack_update_timer = hl.timer(function()
      stack_update_timer = nil
      update_nstack_count()
    end, { timeout = 25, type = "oneshot" })
  end

  local function dismiss_monocle_notice()
    if monocle_notice and monocle_notice:is_alive() then
      monocle_notice:dismiss()
    end
    monocle_notice = nil
  end

  local function update_monocle_notice(workspace)
    if current_layout ~= monocle_layout then
      dismiss_monocle_notice()
      return
    end

    workspace = workspace or hl.get_active_workspace()
    local count = tiled_window_count(workspace)
    if count <= 1 then
      dismiss_monocle_notice()
      return
    end

    local text = "Monocle: " .. tostring(count) .. " windows"
    if monocle_notice and monocle_notice:is_alive() then
      monocle_notice:set_text(text)
      monocle_notice:set_timeout(60000)
      monocle_notice:pause()
    else
      monocle_notice = hl.notification.create({
        text = text,
        duration = 60000,
        icon = notification_icons.info,
        color = "rgba(edb443ff)",
        font_size = 13,
      })
      monocle_notice:pause()
    end
  end

  local function layout_name(layout)
    return layout_names[layout] or tostring(layout)
  end

  local function notify_layout(layout)
    hl.notification.create({
      text = "Layout: " .. layout_name(layout),
      duration = 1200,
      icon = notification_icons.info,
      color = "rgba(edb443ff)",
      font_size = 13,
    })
  end

  local function set_layout(layout)
    if not apply_workspace_layout(active_workspace(), layout) then
      return false
    end
    current_layout = layout
    write_layout_state()

    if layout == quadrants_layout then
      dismiss_monocle_notice()
      schedule_nstack_count_update()
    elseif is_nstack_layout(layout) then
      dismiss_monocle_notice()
      schedule_nstack_count_update()
    else
      update_monocle_notice()
    end
    return true
  end

  _G.im_hyprland_set_layout = function(layout)
    if not layout_names[layout] then
      hl.notification.create({
        text = "Unknown layout: " .. tostring(layout),
        duration = 1800,
        icon = notification_icons.warning,
        color = "rgba(edb443ff)",
        font_size = 13,
      })
      return
    end

    if set_layout(layout) then
      notify_layout(layout)
    end
  end

  local function sync_layout_for_active_workspace(workspace)
    workspace = workspace or active_workspace()
    current_layout = workspace_layout(workspace)
    write_layout_state(workspace)

    if current_layout == quadrants_layout then
      dismiss_monocle_notice()
      schedule_nstack_count_update()
    elseif is_nstack_layout(current_layout) then
      dismiss_monocle_notice()
      schedule_nstack_count_update()
    else
      update_monocle_notice(workspace)
    end
  end

  local function cycle_layout(delta)
    local current_index = 1
    for index, layout in ipairs(layout_cycle) do
      if layout == current_layout then
        current_index = index
        break
      end
    end

    local next_index = ((current_index - 1 + delta) % #layout_cycle) + 1
    local next_layout = layout_cycle[next_index]
    if set_layout(next_layout) then
      notify_layout(next_layout)
    end
  end

  local function toggle_columns_monocle()
    if current_layout == columns_layout then
      set_layout(monocle_layout)
    else
      set_layout(columns_layout)
    end
  end

  local function active_group_size()
    local window = hl.get_active_window()
    return window and window.group and window.group.size or 0
  end

  local function monocle_next()
    local window = hl.get_active_window()
    if window and window.group and window.group.size and window.group.size > 1 then
      dispatch(hl.dsp.group.next({ window = window_selector(window) }))
    elseif current_layout == monocle_layout or current_layout == tabbed_layout then
      dispatch(hl.dsp.layout("cyclenext"))
      update_monocle_notice()
    else
      dispatch(hl.dsp.window.cycle_next({ next = true, tiled = true, floating = false }))
    end
  end

  local function monocle_prev()
    local window = hl.get_active_window()
    if window and window.group and window.group.size and window.group.size > 1 then
      dispatch(hl.dsp.group.prev({ window = window_selector(window) }))
    elseif current_layout == monocle_layout or current_layout == tabbed_layout then
      dispatch(hl.dsp.layout("cycleprev"))
      update_monocle_notice()
    else
      dispatch(hl.dsp.window.cycle_next({ next = false, tiled = true, floating = false }))
    end
  end

  local function focus_direction(direction)
    overview_trace("focus_direction " .. direction)
    if active_group_size() > 1 or current_layout == monocle_layout or current_layout == tabbed_layout then
      if direction == "up" or direction == "left" then
        monocle_prev()
      else
        monocle_next()
      end
      return
    end

    dispatch(hl.dsp.focus({ direction = direction }))
  end

  local function swap_direction(direction)
    if current_layout == tabbed_layout then
      dispatch(hl.dsp.layout("movetab " .. ((direction == "left" or direction == "up") and "-1" or "1")))
      return
    end
    if enable_nstack and is_nstack_layout(current_layout) and active_group_size() <= 1 then
      dispatch(hl.dsp.layout("swapdirection " .. direction))
      return
    end

    dispatch(hl.dsp.window.swap({ direction = direction }))
  end

  focus_workspace = function(workspace_id)
    dispatch(hl.dsp.focus({ workspace = tostring(workspace_id), on_current_monitor = true }))
  end

  move_window_to_workspace = function(workspace_id, follow, window)
    local target_window = window or hl.get_active_window()
    local target_selector = window_selector(target_window)
    dispatch(hl.dsp.window.move({ workspace = tostring(workspace_id), follow = false, window = target_selector }))
    if follow then
      focus_workspace(workspace_id)
      if target_selector then
        dispatch(hl.dsp.focus({ window = target_selector }))
      end
    end
  end

  local function toggle_focus_layout()
    local workspace = active_workspace()
    set_layout(workspace_layout(workspace) == tabbed_layout and focus_return_layout(workspace) or tabbed_layout)
  end

  local function force_columns_layout()
    set_layout(columns_layout)
  end

  local function cycle_layout_or_exit_focus()
    if current_layout == tabbed_layout then
      set_layout(focus_return_layout(active_workspace()))
    else
      cycle_layout(1)
    end
  end

  local function copy_windows(workspace)
    local windows = {}
    if not workspace then
      return windows
    end

    for _, window in ipairs(hl.get_workspace_windows(workspace)) do
      if window and not window.hidden then
        windows[#windows + 1] = window
      end
    end

    return windows
  end

  local function swap_current_workspace_with(target_id)
    local current = active_workspace()
    if not current or not current.id or current.id == target_id then
      return
    end

    local target = hl.get_workspace(tostring(target_id))
    local current_windows = copy_windows(current)
    local target_windows = copy_windows(target)

    for _, window in ipairs(current_windows) do
      move_window_to_workspace(target_id, false, window)
    end

    for _, window in ipairs(target_windows) do
      move_window_to_workspace(current.id, false, window)
    end

    focus_workspace(current.id)
  end

  local function enter_workspace_swap_mode()
    hl.notification.create({
      text = "Swap with workspace 1-9",
      duration = 2200,
      icon = notification_icons.info,
      color = "rgba(edb443ff)",
      font_size = 13,
    })
    dispatch(hl.dsp.submap("swap-workspace"))
  end

  local function focus_next_empty_workspace()
    local workspace_id = find_empty_workspace(hl.get_active_monitor(), active_workspace_id())
    if workspace_id then
      focus_workspace(workspace_id)
    end
  end

  local function move_to_next_empty_workspace(follow)
    local window = hl.get_active_window()
    if not window then
      return
    end

    local workspace_id = find_empty_workspace(hl.get_active_monitor(), active_workspace_id())
    if workspace_id then
      move_window_to_workspace(workspace_id, follow, window)
    end
  end

  local function cycle_workspace(delta)
    local current = active_workspace_id()
    local next_workspace = ((current - 1 + delta) % max_workspace) + 1
    focus_workspace(next_workspace)
  end

  local function move_window_to_monitor(direction, follow)
    local window = hl.get_active_window()
    if not window then
      return
    end

    local original_monitor = hl.get_active_monitor()
    dispatch(hl.dsp.window.move({ monitor = direction, follow = follow, window = window_selector(window) }))

    if not follow and original_monitor then
      dispatch(hl.dsp.focus({ monitor = original_monitor }))
    end
  end

  local function move_window_to_empty_workspace_on_monitor(direction)
    local window = hl.get_active_window()
    local original_monitor = hl.get_active_monitor()
    local target_monitor = hl.get_monitor(direction)

    if not window or not original_monitor or not target_monitor or target_monitor == original_monitor then
      return
    end

    local workspace_id = find_empty_workspace(target_monitor, active_workspace_id())
    if not workspace_id then
      return
    end

    dispatch(hl.dsp.focus({ monitor = target_monitor }))
    focus_workspace(workspace_id)
    dispatch(hl.dsp.focus({ monitor = original_monitor }))
    move_window_to_workspace(workspace_id, false, window)
  end

  ctx.is_nstack_layout = is_nstack_layout
  ctx.hyprland_layout = hyprland_layout
  ctx.configure_quadrants_master = configure_quadrants_master
  ctx.update_nstack_count = update_nstack_count
  ctx.schedule_nstack_count_update = schedule_nstack_count_update
  ctx.dismiss_monocle_notice = dismiss_monocle_notice
  ctx.update_monocle_notice = update_monocle_notice
  ctx.layout_name = layout_name
  ctx.notify_layout = notify_layout
  ctx.set_layout = set_layout
  ctx.sync_layout_for_active_workspace = sync_layout_for_active_workspace
  ctx.cycle_layout = cycle_layout
  ctx.toggle_columns_monocle = toggle_columns_monocle
  ctx.active_group_size = active_group_size
  ctx.monocle_next = monocle_next
  ctx.monocle_prev = monocle_prev
  ctx.focus_direction = focus_direction
  ctx.swap_direction = swap_direction
  ctx.focus_workspace = focus_workspace
  ctx.move_window_to_workspace = move_window_to_workspace
  ctx.toggle_focus_layout = toggle_focus_layout
  ctx.force_columns_layout = force_columns_layout
  ctx.cycle_layout_or_exit_focus = cycle_layout_or_exit_focus
  ctx.copy_windows = copy_windows
  ctx.swap_current_workspace_with = swap_current_workspace_with
  ctx.enter_workspace_swap_mode = enter_workspace_swap_mode
  ctx.focus_next_empty_workspace = focus_next_empty_workspace
  ctx.move_to_next_empty_workspace = move_to_next_empty_workspace
  ctx.cycle_workspace = cycle_workspace
  ctx.move_window_to_monitor = move_window_to_monitor
  ctx.move_window_to_empty_workspace_on_monitor = move_window_to_empty_workspace_on_monitor
end

return M
