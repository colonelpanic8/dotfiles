local M = {}

function M.setup(ctx)
  local _ENV = ctx
  local fullscreen_states = {}

  local function unset_fullscreen_state(window, state)
    dispatch(hl.dsp.window.fullscreen_state({
      internal = state.internal,
      client = state.client,
      action = "unset",
      window = window_selector(window),
    }))
  end

  local function reconcile_fullscreen_state(window)
    if not window or not window.address then
      return
    end

    local address = tostring(window.address)
    local previous = fullscreen_states[address]
    local current = {
      internal = tonumber(window.fullscreen) or 0,
      client = tonumber(window.fullscreen_client) or 0,
    }
    fullscreen_states[address] = current

    if window.floating or current_layout == monocle_layout then
      return
    end

    if current.internal == 1 or (previous and previous.internal >= 2 and current.internal > 0 and current.client == 0) then
      unset_fullscreen_state(window, current)
      fullscreen_states[address] = { internal = 0, client = 0 }
    end
  end

  hl.on("hyprland.start", function()
    apply_nstack_config()
    apply_hyprexpo_config()
    apply_hyprwinview_config()
    apply_hyprglass_config()
    apply_rules()
    hl.exec_cmd("sh -lc '/run/current-system/sw/bin/uwsm finalize HYPRLAND_INSTANCE_SIGNATURE XDG_CURRENT_DESKTOP XDG_SESSION_DESKTOP XDG_SESSION_TYPE XAUTHORITY IMALISON_SESSION_TYPE=wayland IMALISON_WINDOW_MANAGER=hyprland || dbus-update-activation-environment --systemd XDG_RUNTIME_DIR WAYLAND_DISPLAY DISPLAY XAUTHORITY HYPRLAND_INSTANCE_SIGNATURE XDG_CURRENT_DESKTOP XDG_SESSION_DESKTOP XDG_SESSION_TYPE IMALISON_SESSION_TYPE IMALISON_WINDOW_MANAGER; systemctl --user start hyprland-session.target'")
    hl.exec_cmd("hypridle")
    hl.exec_cmd("wl-paste --type text --watch cliphist store")
    hl.exec_cmd("wl-paste --type image --watch cliphist store")
    write_layout_state()
    schedule_nstack_count_update()
    refresh_monitor_reserved_cache(0.25)
    refresh_monitor_reserved_cache(1.25)
  end)

  hl.on("config.reloaded", apply_nstack_config)
  hl.on("config.reloaded", apply_hyprexpo_config)
  hl.on("config.reloaded", apply_hyprwinview_config)
  hl.on("config.reloaded", apply_hyprglass_config)
  hl.on("config.reloaded", apply_rules)
  hl.on("config.reloaded", refresh_shell_workarea_and_scratchpads)
  hl.on("layer.opened", refresh_shell_workarea_and_scratchpads)
  hl.on("layer.closed", refresh_shell_workarea_and_scratchpads)
  hl.on("monitor.added", refresh_shell_workarea_and_scratchpads)
  hl.on("monitor.removed", refresh_shell_workarea_and_scratchpads)
  hl.on("monitor.layout_changed", refresh_shell_workarea_and_scratchpads)

  hl.on("window.open", schedule_nstack_count_update)
  hl.on("window.destroy", schedule_nstack_count_update)
  hl.on("window.kill", schedule_nstack_count_update)
  hl.on("window.move_to_workspace", schedule_nstack_count_update)
  hl.on("workspace.active", sync_layout_for_active_workspace)
  hl.on("monitor.focused", sync_layout_for_active_workspace)

  hl.on("window.open", update_monocle_notice)
  hl.on("window.destroy", update_monocle_notice)
  hl.on("window.kill", update_monocle_notice)
  hl.on("window.move_to_workspace", update_monocle_notice)
  hl.on("window.fullscreen", reconcile_fullscreen_state)
  hl.on("window.update_rules", reconcile_fullscreen_state)

  hl.on("window.open", adopt_matching_scratchpad_window)
  hl.on("window.class", adopt_matching_scratchpad_window)
  hl.on("window.title", adopt_matching_scratchpad_window)
end

return M
