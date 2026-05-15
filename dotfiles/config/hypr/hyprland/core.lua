local M = {}

function M.setup(ctx)
  local _ENV = ctx
  local function command_line_contains(needle)
    local command_line = io.open("/proc/self/cmdline", "rb")
    if not command_line then
      return false
    end

    local contents = command_line:read("*a") or ""
    command_line:close()
    return contents:find(needle, 1, true) ~= nil
  end

  verify_config = command_line_contains("--verify-config")
  dev_session = os.getenv("IMALISON_HYPRLAND_DEV_SESSION") == "1"

  local function exec(command)
    return hl.dsp.exec_cmd(command)
  end

  local function dispatch(dispatcher)
    return hl.dispatch(dispatcher)
  end

  local action_registry = {}

  local function action_text(value)
    return tostring(value or ""):gsub("[\t\r\n]", " "):gsub(" +", " "):match("^%s*(.-)%s*$")
  end

  local function action_registry_path()
    local runtime_dir = os.getenv("XDG_RUNTIME_DIR") or "/tmp"
    return runtime_dir .. "/hyprland-actions.tsv"
  end

  local function register_action(keys, dispatcher, opts)
    local description = opts and opts.description
    if not description or description == "" then
      return
    end

    local id = tostring(#action_registry + 1)
    action_registry[#action_registry + 1] = {
      id = id,
      keys = action_text(keys),
      description = action_text(description),
      dispatcher = dispatcher,
    }
  end

  local function bind(keys, dispatcher, opts)
    hl.bind(keys, dispatcher, opts)
    register_action(keys, dispatcher, opts)
  end

  _G.im_hyprland_write_actions = function()
    local actions_file = io.open(action_registry_path(), "w")
    if not actions_file then
      return
    end

    for _, action in ipairs(action_registry) do
      actions_file:write(action.id, "\t", action.description, "\t", action.keys, "\n")
    end

    actions_file:close()
  end

  _G.im_hyprland_run_action = function(id)
    local action = action_registry[tonumber(id)]
    if not action then
      return
    end

    if type(action.dispatcher) == "function" then
      action.dispatcher()
    else
      dispatch(action.dispatcher)
    end
  end

  local function shell_quote(value)
    return "'" .. tostring(value):gsub("'", "'\\''") .. "'"
  end

  local function overview_trace(label)
    local enabled = io.open(overview_trace_enabled_path, "r")
    if not enabled then
      return
    end
    enabled:close()

    local trace = io.open(overview_trace_path, "a")
    if trace then
      trace:write(os.date("%Y-%m-%d %H:%M:%S "), label, "\n")
      trace:close()
    end
  end

  local function window_selector(window)
    if not window or not window.address then
      return nil
    end
    return "address:" .. tostring(window.address)
  end

  local function hyprexpo_call(method, arg)
    return function()
      overview_trace("hyprexpo:" .. method .. (arg and (" " .. tostring(arg)) or ""))
      if hl.plugin and hl.plugin.hyprexpo and hl.plugin.hyprexpo[method] then
        hl.plugin.hyprexpo[method](arg)
      else
        hl.notification.create({
          text = "hyprexpo is not loaded",
          duration = 1800,
          icon = notification_icons.warning,
          color = "rgba(edb443ff)",
          font_size = 13,
        })
      end
    end
  end

  local function hyprexpo(action)
    return hyprexpo_call("expo", action or "toggle")
  end

  local function hyprexpo_dispatch(dispatcher, arg)
    return hyprexpo_call(dispatcher, arg)
  end

  local function hyprwinview(action)
    return function()
      local label = "hyprwinview"
      if type(action) == "table" and action.action then
        label = label .. " " .. tostring(action.action)
      elseif type(action) ~= "table" and action ~= nil then
        label = label .. " " .. tostring(action)
      end

      local function invoke()
        overview_trace(label)
        if hl.plugin and hl.plugin.hyprwinview and hl.plugin.hyprwinview.overview then
          hl.plugin.hyprwinview.overview(action)
        else
          hl.notification.create({
            text = "hyprwinview is not loaded",
            duration = 1800,
            icon = notification_icons.warning,
            color = "rgba(edb443ff)",
            font_size = 13,
          })
        end
      end

      invoke()
    end
  end

  local function workspacehistory(action, arg)
    return function()
      if hl.plugin and hl.plugin.workspacehistory and hl.plugin.workspacehistory[action] then
        hl.plugin.workspacehistory[action](arg)
      else
        hl.notification.create({
          text = "workspacehistory is not loaded",
          duration = 1800,
          icon = notification_icons.warning,
          color = "rgba(edb443ff)",
          font_size = 13,
        })
      end
    end
  end

  local function hyprspace(action)
    return function()
      local request = action
      if type(request) == "table" then
        request.action = request.action or "toggle"
        if request.all == nil then
          request.all = true
        end
      else
        request = { action = action or "toggle", all = true }
      end

      overview_trace("hyprspace " .. tostring(request.action))
      if hl.plugin and hl.plugin.hyprspace and hl.plugin.hyprspace.overview then
        hl.plugin.hyprspace.overview(request)
      else
        hl.notification.create({
          text = "hyprspace is not loaded",
          duration = 1800,
          icon = notification_icons.warning,
          color = "rgba(edb443ff)",
          font_size = 13,
        })
      end
    end
  end

  local function apply_nstack_config()
    if verify_config or not enable_nstack or not configure_nstack_plugin_from_lua then
      return
    end

    hl.config({
      plugin = {
        nstack = {
          layout = {
            orientation = "left",
            new_on_top = false,
            new_near_focused = true,
            new_is_master = false,
            no_gaps_when_only = true,
            special_scale_factor = 0.8,
            inherit_fullscreen = true,
            stacks = 1,
            center_single_master = false,
            mfact = 0.0,
            single_mfact = 1.0,
          },
        },
      },
    })
  end

  local function apply_hyprexpo_config()
    if verify_config or not enable_hyprexpo then
      return
    end

    hl.config({
      plugin = {
        hyprexpo = {
          columns = 3,
          gap_size = 5,
          gap_size_outer = 0,
          bg_col = 0xff111111,
          workspace_method = "center current",
          skip_empty = false,
          gesture_distance = 200,
          keynav_wrap_h = 1,
          keynav_wrap_v = 1,
          keynav_reading_order = 0,
          border_width = 2,
          border_color_current = "rgb(66ccff)",
          border_color_focus = "rgb(edb443)",
          border_color_hover = "rgb(aabbcc)",
          tile_rounding = 5,
          tile_rounding_power = 2.0,
          label_enable = 1,
          label_font_size = 28,
          label_text_mode = "id",
          label_position = "center",
          label_offset_x = 6,
          label_offset_y = 6,
          selection_label_enable = 1,
          selection_label_token_map = "a,s,d,f,g,q,w,e,r,t,z,x,c,v,b",
          selection_label_position = "top-right",
          selection_label_offset_x = 6,
          selection_label_offset_y = 6,
          selection_label_color = 0xffedb443,
          label_show = "always",
          label_color_default = 0xffffffff,
          label_color_hover = 0xffeeeeee,
          label_color_focus = 0xffedb443,
          label_color_current = 0xff66ccff,
          label_bg_enable = 1,
          label_bg_color = 0xcc000000,
          label_bg_rounding = 10,
          label_padding = 12,
          label_font_bold = 1,
          label_pixel_snap = 1,
        },
      },
    })
  end

  local function apply_hyprwinview_config()
    if verify_config or not enable_hyprwinview then
      return
    end

    hl.config({
      plugin = {
        hyprwinview = {
          gap_size = 24,
          margin = 48,
          background = "rgba(10101400)",
          background_blur = 1,
          border_col = "rgba(ffffff33)",
          hover_border_col = "rgba(66ccffee)",
          border_size = 3,
          window_order = "application",
          keys_default_action = "return,enter,space,g,f",
          keys_filter_toggle = "/",
          show_app_icon = 1,
          app_icon_size = 48,
          app_icon_theme_source = "auto",
          app_icon_position = "bottom right",
          app_icon_margin_x = 12,
          app_icon_margin_y = 12,
          app_icon_margin_relative_x = 0.0,
          app_icon_margin_relative_y = 0.0,
          app_icon_offset_x = 0,
          app_icon_offset_y = 0,
          app_icon_backplate_col = "rgba(00000066)",
          app_icon_backplate_padding = 6,
          show_window_text = 1,
          window_text_font = "Sans",
          window_text_size = 14,
          window_text_color = "rgba(ffffffff)",
          window_text_backplate_col = "rgba(00000099)",
          window_text_padding = 6,
          filter_animation_ms = 140,
          animation = "workspace_zoom",
          animation_in_ms = 280,
          animation_out_ms = 220,
          animation_speed = 1.0,
          animation_scale = 0.94,
          animation_stagger_ms = 16,
          animation_stagger_max_ms = 120,
        },
      },
    })

    if hl.plugin and hl.plugin.hyprwinview and hl.plugin.hyprwinview.configure then
      hl.plugin.hyprwinview.configure({
        keys = {
          left = { "a", "h", "left" },
          right = { "d", "l", "right" },
          up = { "w", "k", "up" },
          down = { "s", "j", "down" },
          default_action = { "return", "enter", "space", "g", "f" },
          bring = { "b", "shift+return", "shift+space" },
          bring_replace = { "shift + b" },
          close = { "escape", "q" },
          filter_toggle = { "/" },
        },
      })
    end
  end

  local function active_workspace()
    return hl.get_active_workspace()
  end

  local function active_workspace_id()
    local workspace = active_workspace()
    if workspace and type(workspace.id) == "number" and workspace.id >= 1 then
      return math.min(max_workspace, math.max(1, workspace.id))
    end
    return 1
  end

  local function workspace_key(workspace)
    workspace = workspace or active_workspace()
    if workspace and workspace.id then
      return tostring(workspace.id)
    end
    return tostring(active_workspace_id())
  end

  local function current_workspace_layout()
    return workspace_layouts[workspace_key()] or columns_layout
  end

  local function write_layout_state()
    local runtime_dir = os.getenv("XDG_RUNTIME_DIR")
    if not runtime_dir then
      return
    end

    local file = io.open(runtime_dir .. "/hyprland-layout-state", "w")
    if not file then
      return
    end

    local workspace = active_workspace()
    file:write("workspace=", workspace_key(workspace), "\n")
    file:write("layout=", current_layout, "\n")
    for key, layout in pairs(workspace_layouts) do
      file:write("workspace.", tostring(key), "=", tostring(layout), "\n")
    end
    file:close()
  end

  local function is_normal_workspace(workspace)
    return workspace and not workspace.special and workspace.id and workspace.id >= 1
  end

  local function same_workspace(left, right)
    if not left or not right then
      return false
    end

    if left.name and right.name and tostring(left.name) == tostring(right.name) then
      return true
    end

    return left.id and right.id and left.id == right.id
  end

  local function is_minimized_workspace(workspace)
    if not workspace then
      return false
    end

    local name = tostring(workspace.name or "")
    return name == minimized_workspace or name == "minimized" or (workspace.special and name:find("minimized", 1, true) ~= nil)
  end

  local function is_minimized_window(window)
    return window and is_minimized_workspace(window.workspace)
  end

  local function is_normal_window(window)
    return window
      and window.mapped ~= false
      and not window.hidden
      and window.workspace
      and is_normal_workspace(window.workspace)
      and not is_scratchpad_window(window)
      and not is_minimized_window(window)
  end

  local function tiled_windows(workspace)
    local windows = {}
    if not workspace then
      return windows
    end

    for _, window in ipairs(hl.get_workspace_windows(workspace)) do
      if not window.floating and not window.hidden then
        windows[#windows + 1] = window
      end
    end

    return windows
  end

  local function tiled_window_count(workspace)
    return #tiled_windows(workspace)
  end

  local function sort_windows_by_focus_history(windows)
    table.sort(windows, function(left, right)
      return (left.focus_history_id or 0) < (right.focus_history_id or 0)
    end)
  end

  local function window_address_set(windows)
    local addresses = {}
    for _, window in ipairs(windows) do
      if window and window.address then
        addresses[window.address] = true
      end
    end
    return addresses
  end

  local function window_address_list(windows)
    local addresses = {}
    for _, window in ipairs(windows) do
      if window and window.address then
        addresses[#addresses + 1] = window.address
      end
    end
    return addresses
  end

  local function window_address_in_set(window, addresses)
    return window and window.address and addresses[window.address] or false
  end

  local function windows_by_address()
    local windows = {}
    for _, window in ipairs(hl.get_windows()) do
      if window and window.address then
        windows[window.address] = window
      end
    end
    return windows
  end

  local function numeric_component(value, key, index)
    if type(value) ~= "table" then
      return 0
    end

    return tonumber(value[key] or value[index]) or 0
  end

  local function window_center(window)
    local at = window and window.at or {}
    local size = window and window.size or {}
    return numeric_component(at, "x", 1) + numeric_component(size, "x", 1) / 2,
      numeric_component(at, "y", 2) + numeric_component(size, "y", 2) / 2
  end

  local function tiled_window_geometry(window)
    if not window or window.floating then
      return nil
    end

    local selector = window_selector(window)
    if not selector then
      return nil
    end

    local at = window.at or {}
    local size = window.size or {}
    local width = math.floor(numeric_component(size, "x", 1))
    local height = math.floor(numeric_component(size, "y", 2))
    if width <= 0 or height <= 0 then
      return nil
    end

    return {
      selector = selector,
      x = math.floor(numeric_component(at, "x", 1)),
      y = math.floor(numeric_component(at, "y", 2)),
      width = width,
      height = height,
    }
  end

  local function window_distance_squared(window, x, y)
    local wx, wy = window_center(window)
    local dx = wx - x
    local dy = wy - y
    return dx * dx + dy * dy
  end

  local function sort_windows_by_visual_position(windows)
    table.sort(windows, function(left, right)
      local left_x, left_y = window_center(left)
      local right_x, right_y = window_center(right)

      if math.abs(left_x - right_x) > 10 then
        return left_x < right_x
      end
      if math.abs(left_y - right_y) > 10 then
        return left_y < right_y
      end
      return tostring(left.address or "") < tostring(right.address or "")
    end)
  end

  local function grouping_direction(window, anchor)
    local wx, wy = window_center(window)
    local ax, ay = window_center(anchor)
    local dx = wx - ax
    local dy = wy - ay

    if math.abs(dx) >= math.abs(dy) then
      return dx >= 0 and "left" or "right"
    end
    return dy >= 0 and "up" or "down"
  end

  local function grouping_directions(window, anchor)
    local primary = grouping_direction(window, anchor)
    local directions = { primary }
    for _, direction in ipairs({ "left", "right", "up", "down" }) do
      if direction ~= primary then
        directions[#directions + 1] = direction
      end
    end
    return directions
  end

  local function workspace_window_count(workspace_id)
    local workspace = hl.get_workspace(tostring(workspace_id))
    if not workspace then
      return 0
    end
    return workspace.windows or tiled_window_count(workspace)
  end

  local function find_empty_workspace(target_monitor, exclude_id)
    local unused_candidate = nil
    local elsewhere_empty_candidate = nil
    local target_monitor_name = target_monitor and target_monitor.name or nil

    for i = 1, max_workspace do
      if i ~= exclude_id then
        local workspace = hl.get_workspace(tostring(i))

        if not workspace then
          unused_candidate = unused_candidate or i
        elseif is_normal_workspace(workspace) and workspace_window_count(i) == 0 then
          local monitor = workspace.monitor
          if target_monitor_name and monitor and monitor.name == target_monitor_name then
            return i
          end
          elsewhere_empty_candidate = elsewhere_empty_candidate or i
        end
      end
    end

    return unused_candidate or elsewhere_empty_candidate
  end

  ctx.command_line_contains = command_line_contains
  ctx.bind = bind
  ctx.exec = exec
  ctx.dispatch = dispatch
  ctx.shell_quote = shell_quote
  ctx.overview_trace = overview_trace
  ctx.window_selector = window_selector
  ctx.hyprexpo = hyprexpo
  ctx.hyprexpo_dispatch = hyprexpo_dispatch
  ctx.hyprwinview = hyprwinview
  ctx.workspacehistory = workspacehistory
  ctx.hyprspace = hyprspace
  ctx.apply_nstack_config = apply_nstack_config
  ctx.apply_hyprexpo_config = apply_hyprexpo_config
  ctx.apply_hyprwinview_config = apply_hyprwinview_config
  ctx.active_workspace = active_workspace
  ctx.active_workspace_id = active_workspace_id
  ctx.workspace_key = workspace_key
  ctx.current_workspace_layout = current_workspace_layout
  ctx.write_layout_state = write_layout_state
  ctx.is_normal_workspace = is_normal_workspace
  ctx.same_workspace = same_workspace
  ctx.is_minimized_workspace = is_minimized_workspace
  ctx.is_minimized_window = is_minimized_window
  ctx.is_normal_window = is_normal_window
  ctx.tiled_windows = tiled_windows
  ctx.tiled_window_count = tiled_window_count
  ctx.sort_windows_by_focus_history = sort_windows_by_focus_history
  ctx.window_address_set = window_address_set
  ctx.window_address_list = window_address_list
  ctx.window_address_in_set = window_address_in_set
  ctx.windows_by_address = windows_by_address
  ctx.numeric_component = numeric_component
  ctx.window_center = window_center
  ctx.tiled_window_geometry = tiled_window_geometry
  ctx.window_distance_squared = window_distance_squared
  ctx.sort_windows_by_visual_position = sort_windows_by_visual_position
  ctx.grouping_direction = grouping_direction
  ctx.grouping_directions = grouping_directions
  ctx.workspace_window_count = workspace_window_count
  ctx.find_empty_workspace = find_empty_workspace
end

return M
