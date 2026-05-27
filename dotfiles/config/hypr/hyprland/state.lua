local shell_ui_command = "hypr_shell_ui"
local columns_layout = "nStack"
local large_main_layout = "master"
local grid_layout = "grid"
local quadrants_layout = "quadrants"
local monocle_layout = "monocle"

return {
  main_mod = "SUPER",
  mod_alt = "SUPER + ALT",
  hyper = "SUPER + CTRL + ALT",

  terminal = "ghostty --gtk-single-instance=false",
  shell_ui_command = shell_ui_command,
  launcher_command = shell_ui_command .. " launcher",
  run_menu = shell_ui_command .. " run",

  -- Hyprland shadows ordinary keybinds after one fires; without transparent,
  -- the first overview chord after a focus-moving bind can be skipped.
  overview_bind_opts = { dont_inhibit = true, transparent = true },
  overview_trace_enabled_path = "/tmp/hypr-overview-bind.enable",
  overview_trace_path = "/tmp/hypr-overview-bind.log",
  notification_icons = {
    warning = 0,
    info = 1,
    hint = 2,
    error = 3,
    confused = 4,
    ok = 5,
    none = 6,
  },

  max_workspace = 9,
  columns_layout = columns_layout,
  large_main_layout = large_main_layout,
  grid_layout = grid_layout,
  quadrants_layout = quadrants_layout,
  monocle_layout = monocle_layout,
  layout_cycle = { columns_layout, large_main_layout, quadrants_layout, grid_layout },
  layout_names = {
    [columns_layout] = "Columns",
    [large_main_layout] = "Large main",
    [quadrants_layout] = "Quadrants",
    [grid_layout] = "Grid",
    [monocle_layout] = "Monocle",
  },
  minimized_workspace = "special:minimized",
  inactive_opacity_override_tag = "no-inactive-opacity",
  tabbed_group_restore_workspace_prefix = "special:tabbed-monocle-restore-",
  current_layout = columns_layout,
  enable_nstack = true,
  enable_hyprexpo = true,
  enable_hyprwinview = true,
  enable_hyprtasking = false,
  enable_workspace_history = true,
  enable_hyprwobbly = true,
  enable_hyprglass = false,
  hypr_visual_performance_mode = false,
  configure_nstack_plugin_from_lua = false,
  workspace_layouts = {},
  minimized_windows = {},
  tabbed_workspace_groups = {},
  quadrants_arranging = false,
  window_picker_mode = nil,
  window_picker_candidates = {},
  stack_update_timer = nil,
  monocle_notice = nil,
}
