local M = {}

function M.setup(ctx)
  local _ENV = ctx
  local file_chooser_class_rule = "^(xdg-desktop-portal-gtk|org\\.freedesktop\\.impl\\.portal\\.desktop\\.gtk)$"
  local file_chooser_title_rule = "^(Open File|Open Files|Save File|Save Files|Save As|Select File|Select Files|Choose File|Choose Files|File Upload|Upload File|Upload Files|Select Folder|Choose Folder|Open Folder|Save Folder)$"

  local function lower_string(value)
    return string.lower(tostring(value or ""))
  end

  local function title_indicates_file_chooser(title)
    title = lower_string(title)
    if title == "" then
      return false
    end

    for _, exact in ipairs({
      "open file",
      "open files",
      "save file",
      "save files",
      "save as",
      "select file",
      "select files",
      "choose file",
      "choose files",
      "file upload",
      "upload file",
      "upload files",
      "select folder",
      "choose folder",
      "open folder",
      "save folder",
    }) do
      if title == exact then
        return true
      end
    end

    return title:find("file chooser", 1, true) ~= nil
      or title:find("file picker", 1, true) ~= nil
  end

  local function class_indicates_file_chooser(class)
    class = lower_string(class)
    return class == "xdg-desktop-portal-gtk"
      or class == "org.freedesktop.impl.portal.desktop.gtk"
  end

  local function is_file_chooser_window(window)
    return window
      and (
        title_indicates_file_chooser(window.title)
        or title_indicates_file_chooser(window.initial_title)
        or class_indicates_file_chooser(window.class)
        or class_indicates_file_chooser(window.initial_class)
      )
  end

  local function raise_file_chooser_window(window)
    if verify_config or not is_file_chooser_window(window) then
      return
    end

    local selector = window_selector(window)
    if not selector then
      return
    end

    dispatch(hl.dsp.window.float({ action = "enable", window = selector }))
    dispatch(hl.dsp.window.center({ window = selector }))
    dispatch(hl.dsp.focus({ window = selector }))
    dispatch(hl.dsp.window.bring_to_top({ window = selector }))
  end

  local function raise_file_chooser_window_later(window, timeout)
    hl.timer(function()
      local refreshed = window and window.address and hl.get_window(window_selector(window)) or window
      raise_file_chooser_window(refreshed)
    end, { timeout = timeout or 50, type = "oneshot" })
  end

  if enable_nstack and not verify_config then
    hl.plugin.load("/run/current-system/sw/lib/libhyprNStack.so")
  end
  if enable_hyprwinview and not verify_config then
    hl.plugin.load("/run/current-system/sw/lib/libhyprwinview.so")
  end
  if enable_hyprtasking and not verify_config then
    hl.plugin.load("/run/current-system/sw/lib/libhyprtasking.so")
  end
  if enable_hyprexpo and not enable_hyprtasking and not verify_config then
    hl.plugin.load("/run/current-system/sw/lib/libhyprexpo.so")
  end
  if enable_workspace_history and not verify_config then
    hl.plugin.load("/run/current-system/sw/lib/libhypr-workspace-history.so")
  end
  if enable_dynamic_cursors and not verify_config then
    hl.plugin.load("/run/current-system/sw/lib/libhypr-dynamic-cursors.so")
  end
  if enable_hyprglass and not verify_config then
    hl.plugin.load("/run/current-system/sw/lib/hyprglass.so")
  end

  hl.env("XCURSOR_SIZE", tostring(hyprland_cursor_size))
  hl.env("HYPRCURSOR_SIZE", tostring(hyprland_cursor_size))
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
      -- Added 2026-07-18: the default auto (2) left a stale "ghost" cursor
      -- image on the hardware cursor plane (Intel iGPU + Nvidia dGPU hybrid).
      no_hardware_cursors = true,
    },
    general = {
      gaps_in = hyprland_gaps_enabled and 5 or 0,
      gaps_out = hyprland_gaps_enabled and 10 or 0,
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
        size = 7,
        passes = 3,
      },
      active_opacity = 1.0,
      inactive_opacity = 0.65,
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
      focus_on_activate = true,
    },
  })

  hl.curve("overshoot", { type = "bezier", points = { { 0.05, 0.9 }, { 0.1, 1.1 } } })
  hl.curve("smoothOut", { type = "bezier", points = { { 0.36, 1 }, { 0.3, 1 } } })
  hl.curve("smoothInOut", { type = "bezier", points = { { 0.42, 0 }, { 0.58, 1 } } })
  hl.curve("linear", { type = "bezier", points = { { 0, 0 }, { 1, 1 } } })
  local spring_time_scale = 5
  local function spring_curve(mass, stiffness, dampening)
    return {
      type = "spring",
      mass = mass,
      stiffness = stiffness * spring_time_scale * spring_time_scale,
      dampening = dampening * spring_time_scale,
    }
  end

  hl.curve("workspaceSpring", spring_curve(2.4, 38, 8))
  hl.curve("windowSpring", spring_curve(2.5, 40, 10))

  local animations = {
    { leaf = "global", enabled = true, speed = 8, bezier = "default" },

    { leaf = "windows", enabled = true, speed = 8, spring = "windowSpring", style = "slide bottom" },
    { leaf = "windowsIn", enabled = true, speed = 8, spring = "windowSpring", style = "slide bottom" },
    { leaf = "windowsOut", enabled = true, speed = 8, spring = "windowSpring", style = "slide bottom" },
    { leaf = "windowsMove", enabled = true, speed = 8, spring = "windowSpring" },

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

    { leaf = "workspaces", enabled = true, speed = 10, spring = "workspaceSpring", style = "slide" },
    { leaf = "workspacesIn", enabled = true, speed = 10, spring = "workspaceSpring", style = "slide" },
    { leaf = "workspacesOut", enabled = true, speed = 10, spring = "workspaceSpring", style = "slide" },
    { leaf = "specialWorkspace", enabled = true, speed = 8, spring = "workspaceSpring", style = "slidevert" },
    { leaf = "specialWorkspaceIn", enabled = true, speed = 8, spring = "workspaceSpring", style = "slidevert" },
    { leaf = "specialWorkspaceOut", enabled = true, speed = 8, spring = "workspaceSpring", style = "slidevert" },

    { leaf = "zoomFactor", enabled = true, speed = 7, bezier = "smoothOut" },
    -- Disabled for now: Hyprland 0.54.0 can crash while damaging a monitor
    -- from this startup animation's update callback during output discovery.
    -- { leaf = "monitorAdded", enabled = true, speed = 5, bezier = "smoothOut" },
    { leaf = "monitorAdded", enabled = false, speed = 5, bezier = "smoothOut" },
  }

  for _, animation in ipairs(animations) do
    hl.animation(animation)
  end

  local function apply_hyprglass_config()
    if verify_config or not enable_hyprglass then
      return
    end

    hl.config({
      plugin = {
        hyprglass = {
          enabled = 0,
          default_theme = "dark",
          default_preset = "default",
        },
      },
    })
  end

  local function apply_dynamic_cursors_config()
    if verify_config or not enable_dynamic_cursors then
      return
    end

    hl.config({
      plugin = {
        dynamic_cursors = {
          enabled = true,
          mode = "tilt",
          threshold = 2,
          tilt = {
            limit = 5000,
            activation = "negative_quadratic",
            window = 100,
            full = 60,
          },
          shake = {
            enabled = true,
            threshold = 6.0,
            base = 4.0,
            speed = 4.0,
            influence = 0.0,
            limit = 0.0,
            timeout = 2000,
            effects = true,
            ipc = false,
          },
          hyprcursor = {
            nearest = 1,
            enabled = true,
            resolution = -1,
            fallback = "clientside",
          },
        },
      },
    })
  end

  local function apply_hyprtasking_config()
    if verify_config or not enable_hyprtasking then
      return
    end

    hl.config({
      plugin = {
        hyprtasking = {
          layout = "grid",
          -- hyprtasking reads bg_color as 0xAARRGGBB and forces it opaque, so
          -- the stock default (0x000000FF) renders as solid blue. Pin true black.
          bg_color = 0xff000000,
          gap_size = 10,
          -- Active workspace tile is bordered with general:col.active_border,
          -- inactive tiles with col.inactive_border; this is the selection
          -- highlight and it follows hyprtasking:move. Keep it clearly visible.
          border_size = 3,
          exit_on_hovered = false,
          close_overview_on_reload = false,
          grid = {
            rows = 3,
            cols = 3,
          },
        },
      },
    })
  end

  local function apply_visual_performance_mode()
    if verify_config then
      return
    end

    local visual_effects_enabled = not hypr_visual_performance_mode
    hl.config({
      decoration = {
        blur = {
          enabled = visual_effects_enabled,
        },
      },
      animations = {
        enabled = visual_effects_enabled,
      },
    })

  end

  local function toggle_visual_performance_mode()
    hypr_visual_performance_mode = not hypr_visual_performance_mode
    apply_visual_performance_mode()
    hl.notification.create({
      text = "Hyprland performance mode: " .. (hypr_visual_performance_mode and "on" or "off"),
      duration = 1800,
      icon = hypr_visual_performance_mode and notification_icons.warning or notification_icons.ok,
      color = hypr_visual_performance_mode and "rgba(edb443ff)" or "rgba(33ccffee)",
      font_size = 13,
    })
  end

  local function apply_rules()
    if verify_config then
      return
    end

    hl.workspace_rule({ workspace = "w[tv1]s[false]", gaps_out = 0, gaps_in = 0 })
    hl.workspace_rule({ workspace = "f[1]s[false]", gaps_out = 0, gaps_in = 0 })

    hl.window_rule({
      name = "tagged-gaming-window",
      match = { tag = gaming_window_tag },
      idle_inhibit = "fullscreen",
      opaque = true,
      no_blur = true,
      no_shadow = true,
      no_anim = true,
      rounding = 0,
      border_size = 0,
    })

    hl.window_rule({ match = { class = "^()$", title = "^()$" }, float = true })
    hl.window_rule({ match = { title = "^(Picture-in-Picture)$" }, float = true })
    hl.window_rule({
      name = "github-clone-picker",
      match = { class = "^(com\\.mitchellh\\.ghostty\\.githubclone)$" },
      float = true,
      center = true,
      focus_on_activate = true,
      stay_focused = true,
    })
    hl.window_rule({
      name = "rofi-glass-window",
      match = { class = "^(rofi)$" },
      float = true,
      center = true,
      decorate = false,
      no_shadow = true,
      xray = false,
    })
    hl.layer_rule({
      name = "rofi-glass-layer",
      match = { namespace = "^(rofi)$" },
      blur = true,
      ignore_alpha = 0.05,
      xray = false,
    })
    hl.window_rule({
      name = "file-chooser-dialogs",
      match = { title = file_chooser_title_rule },
      float = true,
      center = true,
      focus_on_activate = true,
      stay_focused = true,
    })
    hl.window_rule({
      name = "portal-gtk-dialogs",
      match = { class = file_chooser_class_rule },
      float = true,
      center = true,
      focus_on_activate = true,
      stay_focused = true,
    })
    hl.window_rule({ match = { title = "^(Confirm)$" }, float = true })

    -- The AI desktop apps fire xdg-activation requests while streaming
    -- responses; with misc:focus_on_activate=true that steals focus from
    -- whatever window the user is actually working in. focus_on_activate is
    -- a dynamic rule (applies to already-mapped windows on reload);
    -- suppress_event only applies at map time.
    for index, class in ipairs({ "^(com\\.anthropic\\.Claude|claude-desktop)$", "^(codex-desktop)$" }) do
      hl.window_rule({
        name = "ai-app-no-activate-focus-" .. tostring(index),
        match = { class = class },
        focus_on_activate = false,
        suppress_event = "activatefocus",
      })
    end

    for index, match in ipairs({
      { class = "^(flameshot)$" },
      { title = "^(flameshot)$" },
    }) do
      hl.window_rule({
        name = "flameshot-overlay-" .. tostring(index),
        match = match,
        float = true,
        no_anim = true,
        suppress_event = "fullscreen",
      })
    end
    hl.layer_rule({
      name = "flameshot-layer-overlay",
      match = { namespace = "^(flameshot)$" },
      no_anim = true,
    })

    hl.window_rule({
      match = { class = "^(com\\.mitchellh\\.ghostty\\.dropdown)$" },
      no_anim = true,
    })
    hl.window_rule({
      match = { class = "^(com\\.mitchellh\\.ghostty\\.dropdown)$" },
      tag = "+hyprglass_enabled",
    })
    hl.window_rule({
      match = { class = "^(com\\.mitchellh\\.ghostty\\.dropdown)$" },
      tag = "+hyprglass_theme_light",
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
    -- Codex's pet is a separate floating surface from the main ChatGPT
    -- window. Keep it available across workspaces, but preserve the surface's
    -- own alpha channel without compositor blur, dimming, or decorations.
    -- This rule follows the general pinned-window rule so its border override
    -- wins after pinning the pet.
    hl.window_rule({
      name = "codex-pet",
      match = {
        class = "^(codex-desktop)$",
        title = "^(Codex)$",
      },
      float = true,
      pin = true,
      decorate = false,
      no_blur = true,
      no_shadow = true,
      border_size = 0,
      opacity = "1.0 override 1.0 override 1.0 override",
    })
    hl.window_rule({
      match = { tag = inactive_opacity_override_tag },
      opacity = "1.0 override 1.0 override 1.0 override",
    })
  end

  ctx.apply_rules = apply_rules
  ctx.apply_hyprglass_config = apply_hyprglass_config
  ctx.apply_dynamic_cursors_config = apply_dynamic_cursors_config
  ctx.apply_hyprtasking_config = apply_hyprtasking_config
  ctx.apply_visual_performance_mode = apply_visual_performance_mode
  ctx.is_file_chooser_window = is_file_chooser_window
  ctx.raise_file_chooser_window = raise_file_chooser_window
  ctx.raise_file_chooser_window_later = raise_file_chooser_window_later
  ctx.toggle_visual_performance_mode = toggle_visual_performance_mode
end

return M
