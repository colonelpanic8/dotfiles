local M = {}

function M.setup(ctx)
  local _ENV = ctx
  local minimized_workspace = "special:minimized"
  -- Hyprland owns membership (whatever sits on minimized_workspace); this only
  -- remembers restore order, and resets on config reload.
  local restore_order = {}

  local function is_minimized_window(window)
    return window and window.workspace and window.workspace.name == minimized_workspace
  end

  local function minimized_windows()
    local all = hl.get_windows()
    local live = {}
    for _, window in ipairs(all) do
      if is_minimized_window(window) then
        live[window.address] = window
      end
    end

    local windows = {}
    local function take(address)
      local window = live[address]
      if window then
        live[address] = nil
        windows[#windows + 1] = window
      end
    end
    local recorded = {}
    for _, address in ipairs(restore_order) do
      recorded[address] = true
    end
    -- Windows minimized before the last config reload restore after the rest.
    for _, window in ipairs(all) do
      if not recorded[window.address] then
        take(window.address)
      end
    end
    for _, address in ipairs(restore_order) do
      take(address)
    end

    restore_order = {}
    for index, window in ipairs(windows) do
      restore_order[index] = window.address
    end
    return windows
  end

  local function minimize_window(window)
    if not window then
      return
    end

    for index, address in ipairs(restore_order) do
      if address == window.address then
        table.remove(restore_order, index)
        break
      end
    end
    restore_order[#restore_order + 1] = window.address
    move_window_to_workspace(minimized_workspace, false, window)
  end

  local function minimize_active_window()
    minimize_window(hl.get_active_window())
  end

  local function minimize_other_classes()
    local focused = hl.get_active_window()
    local workspace = active_workspace()
    if not focused or not workspace then
      return
    end

    for _, window in ipairs(tiled_windows(workspace)) do
      if window ~= focused and window.class ~= focused.class then
        minimize_window(window)
      end
    end
  end

  local function restore_last_minimized()
    local workspace = active_workspace()
    local windows = minimized_windows()
    local window = windows[#windows]
    if not workspace or not window then
      return
    end

    move_window_to_workspace(workspace.id, false, window)
    dispatch(hl.dsp.focus({ window = window_selector(window) }))
  end

  local function restore_all_minimized()
    local workspace = active_workspace()
    if not workspace then
      return
    end

    local windows = minimized_windows()
    for index = #windows, 1, -1 do
      move_window_to_workspace(workspace.id, false, windows[index])
    end
  end

  local function restore_focused_class()
    local focused = hl.get_active_window()
    local workspace = active_workspace()
    if not focused or not workspace or not focused.class then
      return
    end

    for _, window in ipairs(minimized_windows()) do
      if window.class == focused.class then
        move_window_to_workspace(workspace.id, false, window)
      end
    end
  end

  ctx.is_minimized_window = is_minimized_window
  ctx.minimized_windows = minimized_windows
  ctx.minimize_active_window = minimize_active_window
  ctx.minimize_other_classes = minimize_other_classes
  ctx.restore_last_minimized = restore_last_minimized
  ctx.restore_all_minimized = restore_all_minimized
  ctx.restore_focused_class = restore_focused_class
end

return M
