local M = {}

function M.setup(ctx)
  local _ENV = ctx
  if enable_nstack then
    hl.plugin.load("/run/current-system/sw/lib/libhyprNStack.so")
  end
  if enable_hyprexpo and not verify_config then
    hl.plugin.load("/run/current-system/sw/lib/libhyprexpo.so")
  end
  if enable_hyprwinview and not verify_config then
    hl.plugin.load("/run/current-system/sw/lib/libhyprwinview.so")
  end
  if enable_workspace_history and not verify_config then
    hl.plugin.load("/run/current-system/sw/lib/libhypr-workspace-history.so")
  end

  hl.env("XCURSOR_SIZE", "24")
  hl.env("HYPRCURSOR_SIZE", "24")
  hl.env("QT_QPA_PLATFORMTHEME", "qt5ct")
  hl.env("HYPR_MAX_WORKSPACE", "9")

  hl.config({
    input = {
      kb_layout = "us",
      kb_variant = "",
      kb_model = "",
      kb_options = "",
      kb_rules = "",
      follow_mouse = 1,
      sensitivity = 0,
      touchpad = {
        natural_scroll = false,
      },
    },
    cursor = {
      persistent_warps = true,
    },
    general = {
      gaps_in = 5,
      gaps_out = 10,
      border_size = 2,
      col = {
        active_border = { colors = { "rgba(3b82f6ee)", "rgba(33ccffee)" }, angle = 45 },
        inactive_border = "rgba(00000000)",
      },
      layout = columns_layout,
      allow_tearing = false,
    },
    decoration = {
      rounding = 5,
      blur = {
        enabled = true,
        size = 3,
        passes = 1,
      },
      active_opacity = 1.0,
      inactive_opacity = 0.9,
    },
    animations = {
      enabled = true,
    },
    binds = {
      allow_workspace_cycles = true,
      workspace_back_and_forth = true,
    },
    group = {
      group_on_movetoworkspace = false,
      col = {
        border_active = "rgba(edb443ff)",
        border_inactive = "rgba(091f2eff)",
      },
      groupbar = {
        enabled = true,
        blur = true,
        font_size = 13,
        gradients = true,
        height = 26,
        indicator_gap = 0,
        indicator_height = 1,
        rounding = 5,
        gradient_rounding = 5,
        text_padding = 8,
        col = {
          active = "rgba(edb443ff)",
          inactive = "rgba(101820f2)",
        },
        text_color = "rgba(091018ff)",
        text_color_inactive = "rgba(f2f5f7ff)",
      },
    },
    misc = {
      force_default_wallpaper = 0,
      disable_hyprland_logo = true,
      exit_window_retains_fullscreen = true,
    },
  })

  hl.curve("overshoot", { type = "bezier", points = { { 0.05, 0.9 }, { 0.1, 1.1 } } })
  hl.curve("smoothOut", { type = "bezier", points = { { 0.36, 1 }, { 0.3, 1 } } })
  hl.curve("smoothInOut", { type = "bezier", points = { { 0.42, 0 }, { 0.58, 1 } } })
  hl.curve("linear", { type = "bezier", points = { { 0, 0 }, { 1, 1 } } })

  local animations = {
    { leaf = "global", enabled = true, speed = 8, bezier = "default" },

    { leaf = "windows", enabled = true, speed = 6, bezier = "overshoot", style = "gnomed" },
    { leaf = "windowsIn", enabled = true, speed = 6, bezier = "overshoot", style = "gnomed" },
    { leaf = "windowsOut", enabled = true, speed = 5, bezier = "smoothInOut", style = "gnomed" },
    { leaf = "windowsMove", enabled = true, speed = 6, bezier = "smoothOut" },

    { leaf = "border", enabled = false },
    { leaf = "borderangle", enabled = false },

    { leaf = "fade", enabled = true, speed = 5, bezier = "smoothOut" },
    { leaf = "fadeIn", enabled = true, speed = 5, bezier = "smoothOut" },
    { leaf = "fadeOut", enabled = true, speed = 5, bezier = "smoothOut" },
    { leaf = "fadeSwitch", enabled = true, speed = 5, bezier = "smoothOut" },
    { leaf = "fadeShadow", enabled = true, speed = 5, bezier = "smoothOut" },
    { leaf = "fadeGlow", enabled = true, speed = 5, bezier = "smoothOut" },
    { leaf = "fadeDim", enabled = true, speed = 5, bezier = "smoothOut" },
    { leaf = "fadeLayers", enabled = true, speed = 5, bezier = "smoothOut" },
    { leaf = "fadeLayersIn", enabled = true, speed = 5, bezier = "smoothOut" },
    { leaf = "fadeLayersOut", enabled = true, speed = 5, bezier = "smoothOut" },
    { leaf = "fadePopups", enabled = true, speed = 5, bezier = "smoothOut" },
    { leaf = "fadePopupsIn", enabled = true, speed = 5, bezier = "smoothOut" },
    { leaf = "fadePopupsOut", enabled = true, speed = 5, bezier = "smoothOut" },
    { leaf = "fadeDpms", enabled = true, speed = 5, bezier = "smoothOut" },

    { leaf = "layers", enabled = true, speed = 5, bezier = "smoothOut", style = "fade" },
    { leaf = "layersIn", enabled = true, speed = 5, bezier = "smoothOut", style = "fade" },
    { leaf = "layersOut", enabled = true, speed = 5, bezier = "smoothOut", style = "fade" },

    { leaf = "workspaces", enabled = true, speed = 6, bezier = "smoothOut", style = "slidefade 15%" },
    { leaf = "workspacesIn", enabled = true, speed = 6, bezier = "smoothOut", style = "slidefade 15%" },
    { leaf = "workspacesOut", enabled = true, speed = 6, bezier = "smoothOut", style = "slidefade 15%" },
    { leaf = "specialWorkspace", enabled = true, speed = 6, bezier = "smoothOut", style = "slidevert" },
    { leaf = "specialWorkspaceIn", enabled = true, speed = 6, bezier = "smoothOut", style = "slidevert" },
    { leaf = "specialWorkspaceOut", enabled = true, speed = 6, bezier = "smoothOut", style = "slidevert" },

    { leaf = "zoomFactor", enabled = true, speed = 7, bezier = "smoothOut" },
    -- Disabled for now: Hyprland 0.54.0 can crash while damaging a monitor
    -- from this startup animation's update callback during output discovery.
    -- { leaf = "monitorAdded", enabled = true, speed = 5, bezier = "smoothOut" },
    { leaf = "monitorAdded", enabled = false, speed = 5, bezier = "smoothOut" },
  }

  for _, animation in ipairs(animations) do
    hl.animation(animation)
  end

  local function apply_rules()
    if verify_config then
      return
    end

    hl.workspace_rule({ workspace = "w[tv1]s[false]", gaps_out = 0, gaps_in = 0 })
    hl.workspace_rule({ workspace = "f[1]s[false]", gaps_out = 0, gaps_in = 0 })

    hl.window_rule({ match = { class = "^()$", title = "^()$" }, float = true })
    hl.window_rule({ match = { title = "^(Picture-in-Picture)$" }, float = true })
    hl.window_rule({ match = { title = "^(Open File)$" }, float = true })
    hl.window_rule({ match = { title = "^(Save File)$" }, float = true })
    hl.window_rule({ match = { title = "^(Confirm)$" }, float = true })
    hl.window_rule({
      match = { class = "^(com\\.mitchellh\\.ghostty\\.dropdown)$" },
      animation = "slide top",
    })
    hl.window_rule({
      match = { class = "^(.*[Rr]umno.*)$" },
      float = true,
      pin = true,
      center = true,
      decorate = false,
      no_shadow = true,
    })
    hl.window_rule({
      match = { title = "^(.*[Rr]umno.*)$" },
      float = true,
      pin = true,
      center = true,
      decorate = false,
      no_shadow = true,
    })
    hl.window_rule({
      name = "subtle-pinned-window-border",
      match = { pin = true },
      border_size = 2,
      border_color = "rgba(edb443ff) rgba(ff4d5dcc)",
    })
  end

  ctx.apply_rules = apply_rules
end

return M
