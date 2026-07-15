local M = {}

function M.setup(ctx)
  local _ENV = ctx

  local function desc(description, opts)
    local bind_opts = {}
    for key, value in pairs(opts or {}) do
      bind_opts[key] = value
    end
    bind_opts.description = description
    return bind_opts
  end

  local function setup_launcher_and_app_bindings()
    bind(main_mod .. " + P", exec(launcher_command), desc("Open application launcher"))
    bind(main_mod .. " + SHIFT + P", exec(run_menu), desc("Open command runner"))
    bind(main_mod .. " + SHIFT + Return", exec(terminal), desc("Open terminal"))
    bind(main_mod .. " + E", exec("emacsclient --eval '(emacs-everywhere)'"), desc("Open Emacs Everywhere"))
    bind(main_mod .. " + V", exec("wl-paste --no-newline | ydotool type --file -"), desc("Type clipboard contents"))
  end

  local function setup_shell_and_session_bindings()
    bind(hyper .. " + SHIFT + N", exec(shell_ui_command .. " control-center"), desc("Open control center"))
    bind(hyper .. " + CTRL + N", exec(shell_ui_command .. " settings"), desc("Open system settings"))
    bind(main_mod .. " + Q", exec("hyprctl reload"), desc("Reload Hyprland"))
    bind(main_mod .. " + R", exec("hyprctl reload"), desc("Reload Hyprland"))
    bind(hyper .. " + SHIFT + L", exec("hyprlock"), desc("Lock screen"))
    bind(hyper .. " + SHIFT + V", toggle_visual_performance_mode, desc("Toggle Hyprland performance mode"))
    bind(hyper .. " + slash", function()
      hl.exec_cmd("toggle_taffybar")
      refresh_monitor_reserved_cache(0.25)
      refresh_active_scratchpad_geometries_later(600)
    end, desc("Toggle taffybar"))
  end

  local function setup_audio_media_bindings()
    bind("F5", exec("voxtype record start"), desc("Start hold-to-talk dictation"))
    bind("F5", exec("voxtype record stop"), desc("Stop and transcribe hold-to-talk dictation", { release = true }))
    bind(hyper .. " + B", exec("voxtype record start"), desc("Start hold-to-talk dictation"))
    bind(hyper .. " + B", exec("voxtype record stop"), desc("Stop and transcribe hold-to-talk dictation", { release = true }))
    bind(hyper .. " + Escape", exec("voxtype record cancel"), desc("Cancel voice dictation"))

    bind(main_mod .. " + I", exec("set_volume --unmute --change-volume +5"), desc("Raise volume", { repeating = true }))
    bind(main_mod .. " + K", exec("set_volume --unmute --change-volume -5"), desc("Lower volume", { repeating = true }))
    bind(main_mod .. " + U", exec("set_volume --toggle-mute"), desc("Toggle mute"))
    bind(main_mod .. " + semicolon", exec("playerctl play-pause"), desc("Play or pause media"))
    bind(main_mod .. " + L", exec("playerctl next"), desc("Skip to next media track"))
    bind(main_mod .. " + J", exec("playerctl previous"), desc("Skip to previous media track"))

    bind("XF86AudioPlay", exec("playerctl play-pause"), desc("Play or pause media"))
    bind("XF86AudioPause", exec("playerctl play-pause"), desc("Play or pause media"))
    bind("XF86AudioNext", exec("playerctl next"), desc("Skip to next media track"))
    bind("XF86AudioPrev", exec("playerctl previous"), desc("Skip to previous media track"))
    bind("XF86AudioRaiseVolume", exec("set_volume --unmute --change-volume +5"), desc("Raise volume", { repeating = true }))
    bind("XF86AudioLowerVolume", exec("set_volume --unmute --change-volume -5"), desc("Lower volume", { repeating = true }))
    bind("XF86AudioMute", exec("set_volume --toggle-mute"), desc("Toggle mute"))
    bind(hyper .. " + O", exec("rofi_paswitch"), desc("Open PulseAudio output switcher"))
    bind(hyper .. " + SHIFT + O", exec("kef-optical"), desc("Switch KEF speakers to optical input"))
  end

  local function setup_display_wallpaper_and_capture_bindings()
    bind("XF86MonBrightnessUp", exec("brightness.sh up"), desc("Raise display brightness", { repeating = true }))
    bind("XF86MonBrightnessDown", exec("brightness.sh down"), desc("Lower display brightness", { repeating = true }))
    bind("Print", exec("flameshot gui"), desc("Take screenshot"))
    bind(hyper .. " + H", exec("flameshot gui"), desc("Take screenshot"))
    bind(hyper .. " + backslash", exec("mpg341cx_input toggle"), desc("Toggle monitor input"))
    bind(hyper .. " + comma", exec("rofi_wallpaper.sh"), desc("Open wallpaper menu"))
    bind(hyper .. " + SHIFT + comma", exec("neowall-wallpaper toggle"), desc("Toggle neowall wallpaper"))
  end

  local function setup_rofi_and_tool_bindings()
    bind(main_mod .. " + X", exec("rofi_command.sh"), desc("Open command menu"))
    bind(hyper .. " + V", exec([[cliphist list | rofi -dmenu -p "Clipboard" | cliphist decode | wl-copy]]), desc("Open clipboard history"))
    bind(hyper .. " + P", exec("rofi-pass"), desc("Open password menu"))
    bind(hyper .. " + N", exec("rofi_codex_desktop_project.sh"), desc("Start Codex Desktop thread from project"))
    bind(hyper .. " + T", exec("mova_capture.sh"), desc("Quick-capture task/note into mova (Enter: Today/NEXT, Alt+i: Inbox)"))
    bind(hyper .. " + SHIFT + T", exec("mova_capture.sh --pick"), desc("Quick-capture into mova, picking a capture template"))
    bind(hyper .. " + C", exec("rofi_ai_scratchpad.sh"), desc("Choose AI scratchpad (Codex/Claude)"))
    bind(hyper .. " + SHIFT + C", exec("rofi_tmcodex.sh resume"), desc("Resume Codex session"))
    bind(hyper .. " + L", exec("hypr_rofi_layout"), desc("Open Hyprland layout menu"))
    bind(hyper .. " + K", exec("rofi_kill_process.sh"), desc("Open process kill menu"))
    bind(hyper .. " + SHIFT + K", exec("rofi_kill_all.sh"), desc("Open kill-all menu"))
    bind(hyper .. " + R", exec("rofi_systemd_mono"), desc("Open systemd unit menu"))
    bind(hyper .. " + X", exec("hypr_rofi_action"), desc("Open Hyprland action menu"))
    bind(hyper .. " + I", exec("rofi_select_input.hs"), desc("Open input selection menu"))
    bind(hyper .. " + Y", exec("rofi_agentic_skill"), desc("Open agentic skill menu"))
  end

  local function setup_external_command_bindings()
    setup_launcher_and_app_bindings()
    setup_shell_and_session_bindings()
    setup_audio_media_bindings()
    setup_display_wallpaper_and_capture_bindings()
    setup_rofi_and_tool_bindings()
  end

  local function setup_window_overview_bindings()
    local function hyprtasking_loaded()
      return hl.plugin and hl.plugin.hyprtasking and hl.plugin.hyprtasking.toggle
    end

    local function hyprtasking_active()
      return hyprtasking_loaded()
        and hl.plugin.hyprtasking.is_active
        and hl.plugin.hyprtasking.is_active()
    end

    -- Workspace the overview was opened from, so Escape can cancel back to it
    -- (hyprtasking:move switches the active workspace live as you navigate).
    local hyprtasking_origin_workspace = nil

    local function toggle_hyprtasking()
      if hyprtasking_loaded() then
        if not hyprtasking_active() then
          hyprtasking_origin_workspace = hl.get_active_workspace()
        end
        hl.plugin.hyprtasking.toggle("cursor")
      else
        hl.notification.create({
          text = "hyprtasking is not loaded",
          duration = 1800,
          icon = notification_icons.warning,
          color = "rgba(edb443ff)",
          font_size = 13,
        })
      end
    end

    -- Directional keyboard navigation; only acts while the overview is open so
    -- the arrow keys keep their normal meaning otherwise. The active-border
    -- highlight follows the selection.
    local function hyprtasking_move(direction)
      return function()
        if hyprtasking_active() and hl.plugin.hyprtasking.move then
          hl.plugin.hyprtasking.move(direction)
        end
      end
    end

    -- Confirm: close the overview, staying on the highlighted workspace.
    local function hyprtasking_confirm()
      if hyprtasking_active() then
        hl.plugin.hyprtasking.toggle("cursor")
      end
    end

    -- Cancel: restore the origin workspace, then close.
    local function hyprtasking_cancel()
      if not hyprtasking_active() then
        return
      end
      if hyprtasking_origin_workspace and hyprtasking_origin_workspace.id then
        dispatch(hl.dsp.focus({
          workspace = tostring(hyprtasking_origin_workspace.id),
          on_current_monitor = true,
        }))
      end
      hl.plugin.hyprtasking.toggle("cursor")
    end

    bind(main_mod .. " + SHIFT + C", hl.dsp.window.close(), desc("Close active window"))
    bind(main_mod .. " + SHIFT + Q", hl.dsp.exit(), desc("Exit Hyprland"))
    bind(main_mod .. " + Tab", toggle_hyprtasking, desc("Toggle hyprtasking workspace overview", overview_bind_opts))
    bind(main_mod .. " + SHIFT + Tab", hyprwinview({
      action = "show",
      include_current_workspace = false,
      start_in_filter_mode = true,
      default_action = "bring",
    }), desc("Show all-workspace window overview", overview_bind_opts))
    bind(main_mod .. " + SHIFT + slash", hyprwinview({ action = "toggle-filter" }), desc("Toggle window overview filter", overview_bind_opts))
    bind("ALT + Tab", toggle_hyprtasking, desc("Toggle hyprtasking workspace overview", overview_bind_opts))
    bind(main_mod .. " + Left", hyprtasking_move("left"), desc("Overview: select workspace left", overview_bind_opts))
    bind(main_mod .. " + Down", hyprtasking_move("down"), desc("Overview: select workspace down", overview_bind_opts))
    bind(main_mod .. " + Up", hyprtasking_move("up"), desc("Overview: select workspace up", overview_bind_opts))
    bind(main_mod .. " + Right", hyprtasking_move("right"), desc("Overview: select workspace right", overview_bind_opts))
    bind(main_mod .. " + Return", hyprtasking_confirm, desc("Overview: confirm selected workspace", overview_bind_opts))
    bind("Escape", hyprtasking_cancel, desc("Overview: cancel back to origin workspace", { non_consuming = true }))
    bind(main_mod .. " + G", hyprwinview({
      action = "show",
      start_in_filter_mode = true,
      default_action = "select",
    }), desc("Show window overview", overview_bind_opts))
    bind(main_mod .. " + B", hyprwinview({
      action = "show",
      start_in_filter_mode = true,
      default_action = "bring",
    }), desc("Bring window from overview", overview_bind_opts))
    bind(main_mod .. " + SHIFT + B", hyprwinview({
      action = "show",
      start_in_filter_mode = true,
      default_action = "bring-replace",
    }), desc("Replace active window from overview", overview_bind_opts))
  end

  local function setup_window_focus_and_move_bindings()
    -- While the hyprtasking overview is open, WASD navigates the workspace grid;
    -- otherwise it keeps its normal directional window-focus behavior.
    local function focus_or_overview_move(direction)
      return function()
        if hl.plugin and hl.plugin.hyprtasking and hl.plugin.hyprtasking.is_active
          and hl.plugin.hyprtasking.is_active() and hl.plugin.hyprtasking.move then
          hl.plugin.hyprtasking.move(direction)
        else
          focus_direction(direction)
        end
      end
    end

    bind(main_mod .. " + W", focus_or_overview_move("up"), desc("Focus window above / overview up"))
    bind(main_mod .. " + S", focus_or_overview_move("down"), desc("Focus window below / overview down"))
    bind(main_mod .. " + A", focus_or_overview_move("left"), desc("Focus window left / overview left"))
    bind(main_mod .. " + D", focus_or_overview_move("right"), desc("Focus window right / overview right"))

    bind(main_mod .. " + SHIFT + W", function()
      swap_direction("up")
    end, desc("Swap active window upward"))
    bind(main_mod .. " + SHIFT + S", function()
      swap_direction("down")
    end, desc("Swap active window downward"))
    bind(main_mod .. " + SHIFT + A", function()
      swap_direction("left")
    end, desc("Swap active window left"))
    bind(main_mod .. " + SHIFT + D", function()
      swap_direction("right")
    end, desc("Swap active window right"))

    bind(main_mod .. " + CTRL + W", function()
      move_window_to_monitor("u", false)
    end, desc("Move window to monitor above"))
    bind(main_mod .. " + CTRL + S", function()
      move_window_to_monitor("d", false)
    end, desc("Move window to monitor below"))
    bind(main_mod .. " + CTRL + A", function()
      move_window_to_monitor("l", false)
    end, desc("Move window to monitor on the left"))
    bind(main_mod .. " + CTRL + D", function()
      move_window_to_monitor("r", false)
    end, desc("Move window to monitor on the right"))
    bind(main_mod .. " + CTRL + SHIFT + W", function()
      move_window_to_empty_workspace_on_monitor("u")
    end, desc("Move window to empty workspace on monitor above"))
    bind(main_mod .. " + CTRL + SHIFT + S", function()
      move_window_to_empty_workspace_on_monitor("d")
    end, desc("Move window to empty workspace on monitor below"))
    bind(main_mod .. " + CTRL + SHIFT + A", function()
      move_window_to_empty_workspace_on_monitor("l")
    end, desc("Move window to empty workspace on left monitor"))
    bind(main_mod .. " + CTRL + SHIFT + D", function()
      move_window_to_empty_workspace_on_monitor("r")
    end, desc("Move window to empty workspace on right monitor"))
  end

  local function setup_submap_bindings()
    hl.define_submap("swap-workspace", function()
      for i = 1, 9 do
        local workspace_id = i
        bind(tostring(i), function()
          swap_current_workspace_with(workspace_id)
          dispatch(hl.dsp.submap("reset"))
        end, desc("Swap current workspace with workspace " .. workspace_id))
      end

      bind("Escape", hl.dsp.submap("reset"), desc("Exit workspace swap mode"))
      bind("catchall", hl.dsp.submap("reset"), desc("Exit workspace swap mode"))
    end)

    hl.define_submap("window-picker", function()
      for i = 1, 9 do
        local index = i
        bind(tostring(i), function()
          activate_window_picker_candidate(index)
        end, desc("Activate window picker candidate " .. index))
      end

      bind("Escape", hl.dsp.submap("reset"), desc("Exit window picker"))
      bind("catchall", hl.dsp.submap("reset"), desc("Exit window picker"))
    end)

  end

  local function setup_window_resize_and_monitor_bindings()
    bind(mod_alt .. " + SHIFT + W", hl.dsp.window.resize({ x = 0, y = -50, relative = true }), desc("Shrink window height upward", { repeating = true }))
    bind(mod_alt .. " + SHIFT + S", hl.dsp.window.resize({ x = 0, y = 50, relative = true }), desc("Grow window height downward", { repeating = true }))
    bind(mod_alt .. " + SHIFT + A", hl.dsp.window.resize({ x = -50, y = 0, relative = true }), desc("Shrink window width leftward", { repeating = true }))
    bind(mod_alt .. " + SHIFT + D", hl.dsp.window.resize({ x = 50, y = 0, relative = true }), desc("Grow window width rightward", { repeating = true }))

    bind(hyper .. " + W", hl.dsp.focus({ monitor = "u" }), desc("Focus monitor above"))
    bind(hyper .. " + S", hl.dsp.focus({ monitor = "d" }), desc("Focus monitor below"))
    bind(hyper .. " + A", hl.dsp.focus({ monitor = "l" }), desc("Focus monitor on the left"))
    bind(hyper .. " + D", hl.dsp.focus({ monitor = "r" }), desc("Focus monitor on the right"))
    bind(hyper .. " + SHIFT + W", function()
      move_window_to_monitor("u", true)
    end, desc("Move window to monitor above and follow"))
    bind(hyper .. " + SHIFT + S", function()
      move_window_to_monitor("d", true)
    end, desc("Move window to monitor below and follow"))
    bind(hyper .. " + SHIFT + A", function()
      move_window_to_monitor("l", true)
    end, desc("Move window to left monitor and follow"))
    bind(hyper .. " + SHIFT + D", function()
      move_window_to_monitor("r", true)
    end, desc("Move window to right monitor and follow"))
  end

  local function setup_layout_and_window_state_bindings()
    bind(main_mod .. " + Space", cycle_layout_or_restore_tabbed_group, desc("Cycle workspace layout"))
    bind(main_mod .. " + SHIFT + Space", force_columns_layout, desc("Force columns layout"))
    bind(main_mod .. " + CTRL + Space", gather_workspace_into_tabbed_group, desc("Gather workspace into tabbed group"))
    bind(main_mod .. " + bracketright", monocle_next, desc("Focus next monocle window"))
    bind(main_mod .. " + bracketleft", monocle_prev, desc("Focus previous monocle window"))
    bind(main_mod .. " + F", toggle_active_window_real_fullscreen, desc("Toggle active window real fullscreen"))
    bind(main_mod .. " + SHIFT + F", toggle_active_window_gaming_mode, desc("Toggle active window gaming fullscreen"))
    bind(main_mod .. " + T", tile_or_float_active_window, desc("Tile or float active window"))
    bind(main_mod .. " + O", toggle_pinned_active_window, desc("Toggle pinned active window"))
    bind(main_mod .. " + M", minimize_active_window, desc("Minimize active window"))
    bind(main_mod .. " + SHIFT + M", restore_last_minimized, desc("Restore last minimized window"))
    bind(main_mod .. " + CTRL + SHIFT + M", function()
      enter_window_picker("minimized")
    end, desc("Pick minimized window to restore"))
    bind(main_mod .. " + SHIFT + equal", schedule_nstack_count_update, desc("Update nstack window count"))
    bind(main_mod .. " + CTRL + M", hl.dsp.window.toggle_swallow(), desc("Toggle window swallowing"))
    bind(main_mod .. " + SHIFT + E", function()
      move_to_next_empty_workspace(true)
    end, desc("Move to next empty workspace"))
    bind(main_mod .. " + CTRL + E", function()
      move_to_next_empty_workspace(false)
    end, desc("Move window to next empty workspace"))
    bind(main_mod .. " + apostrophe", focus_next_class, desc("Focus next window class"))
    bind(hyper .. " + 1", toggle_inactive_opacity_for_active_window, desc("Toggle inactive opacity reduction for active window"))
    bind(mod_alt .. " + W", show_active_window_info, desc("Show active window info"))
  end

  local function setup_scratchpad_bindings()
    bind(main_mod .. " + SHIFT + X", hl.dsp.workspace.toggle_special("NSP"), desc("Toggle NSP special workspace"))
    bind(mod_alt .. " + C", toggle_active_ai_scratchpad, desc("Toggle AI scratchpad (Codex/Claude)"))
    bind(mod_alt .. " + SHIFT + C", toggle_backup_ai_scratchpad, desc("Toggle backup AI scratchpad (Codex/Claude)"))
    bind(mod_alt .. " + D", function()
      toggle_scratchpad("discord")
    end, desc("Toggle Discord scratchpad"))
    bind(mod_alt .. " + E", function()
      toggle_scratchpad("element")
    end, desc("Toggle Element scratchpad"))
    bind(mod_alt .. " + H", function()
      toggle_scratchpad("htop")
    end, desc("Toggle htop scratchpad"))
    bind(mod_alt .. " + K", function()
      toggle_scratchpad("slack")
    end, desc("Toggle Slack scratchpad"))
    bind(mod_alt .. " + M", function()
      toggle_scratchpad("messages")
    end, desc("Toggle Messages scratchpad"))
    bind(mod_alt .. " + S", function()
      toggle_scratchpad("spotify")
    end, desc("Toggle Spotify scratchpad"))
    bind(mod_alt .. " + T", function()
      toggle_scratchpad("transmission")
    end, desc("Toggle Transmission scratchpad"))
    bind(mod_alt .. " + V", function()
      toggle_scratchpad("volume")
    end, desc("Toggle volume scratchpad"))
    bind(mod_alt .. " + X", function()
      toggle_scratchpad("x_com")
    end, desc("Toggle X scratchpad"))
    bind(mod_alt .. " + grave", function()
      toggle_scratchpad("dropdown")
    end, desc("Toggle dropdown scratchpad"))
    bind(mod_alt .. " + Space", minimize_other_classes, desc("Minimize other window classes"))
    bind(mod_alt .. " + SHIFT + Space", restore_focused_class, desc("Restore focused window class"))
    bind(mod_alt .. " + Return", restore_all_minimized, desc("Restore all minimized windows"))
  end

  local function setup_workspace_bindings()
    for i = 1, 9 do
      local workspace = tostring(i)
      bind(main_mod .. " + " .. workspace, hl.dsp.focus({ workspace = workspace, on_current_monitor = true }), desc("Focus workspace " .. workspace))
      bind(main_mod .. " + SHIFT + " .. workspace, hl.dsp.window.move({ workspace = workspace, follow = false }), desc("Move window to workspace " .. workspace))
      bind(main_mod .. " + CTRL + " .. workspace, function()
        dispatch(hl.dsp.window.move({ workspace = workspace, follow = false }))
        dispatch(hl.dsp.focus({ workspace = workspace, on_current_monitor = true }))
      end, desc("Move window to workspace " .. workspace .. " and follow"))
    end

    bind(main_mod .. " + backslash", workspacehistory("cycle", 1), desc("Cycle to next workspace in history"))
    bind(main_mod .. " + slash", workspacehistory("cycle", -1), desc("Cycle to previous workspace in history"))
    bind(main_mod .. " + Escape", workspacehistory("cancel"), desc("Cancel workspace history cycle"))
    bind(main_mod .. " + Z", hl.dsp.focus({ monitor = "+1" }), desc("Focus next monitor"))
    bind(main_mod .. " + SHIFT + Z", hl.dsp.window.move({ monitor = "+1" }), desc("Move window to next monitor"))
    bind(main_mod .. " + mouse_down", function()
      cycle_workspace(1)
    end, desc("Cycle to next workspace"))
    bind(main_mod .. " + mouse_up", function()
      cycle_workspace(-1)
    end, desc("Cycle to previous workspace"))
    bind(hyper .. " + E", focus_next_empty_workspace, desc("Focus next empty workspace"))
    bind(hyper .. " + 5", enter_workspace_swap_mode, desc("Enter workspace swap mode"))
    bind(hyper .. " + G", gather_focused_class, desc("Gather focused window class"))
    bind(hyper .. " + SHIFT + backslash", workspacehistory("debug"), desc("Show workspace history debug info"))
  end

  local function setup_mouse_bindings()
    bind(main_mod .. " + mouse:272", float_and_drag_active_window, desc("Float and drag active window"))
    bind(main_mod .. " + mouse:273", float_and_resize_active_window, desc("Float and resize active window"))
    bind(hyper .. " + mouse:272", float_and_resize_active_window, desc("Float and resize active window"))
  end

  local function setup_internal_window_manager_bindings()
    setup_window_overview_bindings()
    setup_window_focus_and_move_bindings()
    setup_submap_bindings()
    setup_window_resize_and_monitor_bindings()
    setup_layout_and_window_state_bindings()
    setup_scratchpad_bindings()
    setup_workspace_bindings()
    setup_mouse_bindings()
  end

  setup_external_command_bindings()
  setup_internal_window_manager_bindings()
end

return M
