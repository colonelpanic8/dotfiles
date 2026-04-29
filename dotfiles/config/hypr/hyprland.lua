local main_mod = "SUPER"
local mod_alt = "SUPER + ALT"
local hyper = "SUPER + CTRL + ALT"

local terminal = "ghostty --gtk-single-instance=false"
local menu = "rofi -show drun -show-icons"
local run_menu = "rofi -show run"

local max_workspace = 9
local scratchpad_top_margin = 60
local columns_layout = "nStack"
local monocle_layout = "monocle"
local minimized_workspace = "special:minimized"
local current_layout = columns_layout
local enable_nstack = true
local enable_hyprexpo = true
local configure_nstack_plugin_from_lua = false
local workspace_layouts = {}
local minimized_windows = {}
local window_picker_mode = nil
local window_picker_candidates = {}
local stack_update_timer = nil
local monocle_notice = nil
local scratchpad_pending = {}
local monitor_workspace_history = {}

local scratchpads = {
  htop = {
    command = "alacritty --class htop-scratch --title htop -e htop",
    class = "htop-scratch",
  },
  volume = {
    command = "pavucontrol",
    class = "org.pulseaudio.pavucontrol",
  },
  spotify = {
    command = "spotify",
    class = "spotify",
  },
  element = {
    command = "element-desktop",
    class = "Element",
  },
  slack = {
    command = "slack",
    class = "Slack",
  },
  transmission = {
    command = "transmission-gtk",
    class = "transmission-gtk",
  },
  dropdown = {
    command = "ghostty --config-file=/home/imalison/.config/ghostty/dropdown",
    class = "com.mitchellh.ghostty.dropdown",
    dropdown = true,
  },
  gmail = {
    command = "google-chrome-stable --new-window https://mail.google.com/mail/u/0/#inbox",
    class = "google-chrome",
    title = "Gmail",
  },
  messages = {
    command = "google-chrome-stable --new-window https://messages.google.com/web/conversations",
    class = "google-chrome",
    title = "Messages",
  },
}

local function command_line_contains(needle)
  local command_line = io.open("/proc/self/cmdline", "rb")
  if not command_line then
    return false
  end

  local contents = command_line:read("*a") or ""
  command_line:close()
  return contents:find(needle, 1, true) ~= nil
end

local verify_config = command_line_contains("--verify-config")

local function bind(keys, dispatcher, opts)
  hl.bind(keys, dispatcher, opts)
end

local function exec(command)
  return hl.dsp.exec_cmd(command)
end

local function hyprexpo(action)
  return function()
    if hl.plugin and hl.plugin.hyprexpo and hl.plugin.hyprexpo.expo then
      hl.plugin.hyprexpo.expo(action)
    else
      hl.notification.create({
        text = "hyprexpo is not loaded",
        duration = 1800,
        icon = "warning",
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
        bg_col = "rgba(111111ff)",
        workspace_method = "center current",
        skip_empty = false,
        show_workspace_numbers = true,
        workspace_number_color = "rgba(edb443ff)",
        gesture_distance = 200,
      },
    },
  })
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

local function lower_contains(value, needle)
  if not needle or needle == "" then
    return true
  end

  value = string.lower(tostring(value or ""))
  needle = string.lower(tostring(needle))
  return value:find(needle, 1, true) ~= nil
end

local function scratchpad_window_matches(window, def)
  return window
    and lower_contains(window.class, def.class)
    and lower_contains(window.title, def.title)
end

local function is_scratchpad_window(window)
  for _, def in pairs(scratchpads) do
    if scratchpad_window_matches(window, def) then
      return true
    end
  end
  return false
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

local function update_nstack_count()
  if not enable_nstack or current_layout ~= columns_layout then
    return
  end

  local workspace = hl.get_active_workspace()
  local count = tiled_window_count(workspace)
  if count == 0 then
    return
  end
  count = math.max(count, 2)
  hl.dsp.layout("setstackcount " .. tostring(count))()
end

local function schedule_nstack_count_update()
  if stack_update_timer then
    stack_update_timer:set_enabled(false)
  end

  stack_update_timer = hl.timer(update_nstack_count, { timeout = 25, type = "oneshot" })
end

local function dismiss_monocle_notice()
  if monocle_notice and monocle_notice:is_alive() then
    monocle_notice:dismiss()
  end
  monocle_notice = nil
end

local function update_monocle_notice()
  if current_layout ~= monocle_layout then
    dismiss_monocle_notice()
    return
  end

  local workspace = hl.get_active_workspace()
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
      icon = "info",
      color = "rgba(edb443ff)",
      font_size = 13,
    })
    monocle_notice:pause()
  end
end

local function set_layout(layout)
  workspace_layouts[workspace_key()] = layout
  current_layout = layout
  hl.config({ general = { layout = layout } })
  write_layout_state()

  if layout == columns_layout then
    dismiss_monocle_notice()
    schedule_nstack_count_update()
  else
    update_monocle_notice()
  end
end

local function sync_layout_for_active_workspace()
  current_layout = current_workspace_layout()
  hl.config({ general = { layout = current_layout } })
  write_layout_state()

  if current_layout == columns_layout then
    dismiss_monocle_notice()
    schedule_nstack_count_update()
  else
    update_monocle_notice()
  end
end

local function toggle_columns_monocle()
  if current_layout == columns_layout then
    set_layout(monocle_layout)
  else
    set_layout(columns_layout)
  end
end

local function monocle_next()
  if current_layout == monocle_layout then
    hl.dsp.layout("cyclenext")()
    update_monocle_notice()
  else
    hl.dsp.window.cycle_next({ next = true, tiled = true, floating = false })()
  end
end

local function monocle_prev()
  if current_layout == monocle_layout then
    hl.dsp.layout("cycleprev")()
    update_monocle_notice()
  else
    hl.dsp.window.cycle_next({ next = false, tiled = true, floating = false })()
  end
end

local function focus_direction(direction)
  if current_layout == monocle_layout then
    if direction == "up" or direction == "left" then
      monocle_prev()
    else
      monocle_next()
    end
    return
  end

  hl.dsp.focus({ direction = direction })()
end

local function focus_workspace(workspace_id)
  hl.dsp.focus({ workspace = tostring(workspace_id), on_current_monitor = true })()
end

local function monitor_key(monitor)
  if not monitor then
    return "unknown"
  end
  return tostring(monitor.name or monitor.id or "unknown")
end

local function remember_workspace_for_monitor(workspace)
  workspace = workspace or active_workspace()
  if not workspace or not workspace.id or workspace.id < 1 then
    return
  end

  local key = monitor_key(workspace.monitor or hl.get_active_monitor())
  local history = monitor_workspace_history[key] or {}
  if history.current ~= workspace.id then
    history.previous = history.current
    history.current = workspace.id
  end
  monitor_workspace_history[key] = history
end

local function focus_previous_workspace_for_monitor()
  local key = monitor_key(hl.get_active_monitor())
  local history = monitor_workspace_history[key]
  if history and history.previous then
    focus_workspace(history.previous)
  else
    hl.dsp.focus({ workspace = "previous_per_monitor" })()
  end
end

local function move_window_to_workspace(workspace_id, follow, window)
  hl.dsp.window.move({ workspace = tostring(workspace_id), follow = follow, window = window })()
  if follow then
    focus_workspace(workspace_id)
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
    icon = "info",
    color = "rgba(edb443ff)",
    font_size = 13,
  })
  hl.dsp.submap("swap-workspace")()
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
  hl.dsp.window.move({ monitor = direction, follow = follow, window = window })()

  if not follow and original_monitor then
    hl.dsp.focus({ monitor = original_monitor })()
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

  hl.dsp.focus({ monitor = target_monitor })()
  focus_workspace(workspace_id)
  hl.dsp.focus({ monitor = original_monitor })()
  move_window_to_workspace(workspace_id, false, window)
end

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
  for _, window in ipairs(minimized_windows) do
    if window and window ~= target then
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
  local hydrated = {}

  for _, window in ipairs(minimized_windows) do
    if window and window.address and not by_address[window.address] then
      by_address[window.address] = true
      hydrated[#hydrated + 1] = window
    end
  end

  for _, window in ipairs(hl.get_windows()) do
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

  local width
  local height
  local x
  local y
  if def.dropdown then
    width = monitor.width
    height = math.floor(monitor.height * 0.5)
    x = monitor.x
    y = monitor.y + scratchpad_top_margin
  else
    width = math.floor(monitor.width * 0.95)
    height = math.min(math.floor(monitor.height * 0.95), monitor.height - scratchpad_top_margin)
    x = monitor.x + math.floor((monitor.width - width) / 2)
    y = monitor.y + scratchpad_top_margin
  end

  hl.dsp.window.float({ action = "enable", window = window })()
  hl.dsp.window.tag({ tag = "+scratchpad", window = window })()
  hl.dsp.window.tag({ tag = "+scratchpad-" .. name, window = window })()
  hl.dsp.window.resize({ x = width, y = height, relative = false, window = window })()
  hl.dsp.window.move({ x = x, y = y, relative = false, window = window })()
  if def.dropdown then
    hl.dsp.window.set_prop({ prop = "border_size", value = "0", window = window })()
    hl.dsp.window.set_prop({ prop = "no_shadow", value = "1", window = window })()
  end
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
  hl.dsp.focus({ window = window })()
  schedule_scratchpad_geometry(name, window, target_monitor or hl.get_active_monitor())
end

local function scratchpad_is_visible(window)
  local workspace = active_workspace()
  return workspace and window and window.workspace == workspace
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
  hl.dsp.submap("reset")()

  if not window then
    return
  end

  if mode == "go" then
    hl.dsp.focus({ window = window })()
    return
  end

  local workspace = active_workspace()
  if mode == "bring" and workspace then
    move_window_to_workspace(workspace.id, false, window)
    hl.dsp.focus({ window = window })()
    return
  end

  if mode == "minimized" and workspace then
    remove_minimized_window(window)
    restore_minimized_window(window, workspace)
    hl.dsp.focus({ window = window })()
    return
  end

  if mode == "replace" then
    local focused = hl.get_active_window()
    if focused and focused ~= window then
      hl.dsp.window.swap({ target = window, window = focused })()
      hl.dsp.focus({ window = window })()
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
      icon = "info",
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
    icon = "info",
    color = "rgba(edb443ff)",
    font_size = 11,
  })
  hl.dsp.submap("window-picker")()
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
    icon = "info",
    color = "rgba(edb443ff)",
    font_size = 13,
  })
end

local function focus_next_class()
  local focused = hl.get_active_window()
  if not focused or not focused.class or focused.class == "" then
    hl.dsp.window.cycle_next({ next = true, tiled = true, floating = false })()
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
    hl.dsp.focus({ window = target })()
  end
end

local function show_active_window_info()
  local window = hl.get_active_window()
  if not window then
    hl.notification.create({
      text = "No active window",
      duration = 1800,
      icon = "info",
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
    "Address: " .. tostring(window.address or ""),
    "PID: " .. tostring(window.pid or ""),
  }

  hl.notification.create({
    text = table.concat(lines, "\n"),
    duration = 5000,
    icon = "info",
    color = "rgba(edb443ff)",
    font_size = 11,
  })
end

local function raise_or_spawn(class_fragment, command)
  local fragment = string.lower(class_fragment)
  for _, window in ipairs(hl.get_windows()) do
    if is_normal_window(window) and window.class and string.find(string.lower(window.class), fragment, 1, true) then
      hl.dsp.focus({ window = window })()
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
      hl.dsp.focus({ window = window })()
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
    local workspace = active_workspace()
    local target_monitor = hl.get_active_monitor()
    for _, window in ipairs(windows) do
      show_scratchpad_window(name, window, workspace, target_monitor)
    end
  end
end

if enable_nstack then
  hl.plugin.load("/run/current-system/sw/lib/libhyprNStack.so")
end
if enable_hyprexpo and not verify_config then
  hl.plugin.load("/run/current-system/sw/lib/libhyprexpo.so")
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
    border_size = 0,
    col = {
      active_border = { colors = { "rgba(edb443ee)", "rgba(33ccffee)" }, angle = 45 },
      inactive_border = "rgba(595959aa)",
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
    col = {
      border_active = "rgba(edb443ff)",
      border_inactive = "rgba(091f2eff)",
    },
    groupbar = {
      enabled = true,
      font_size = 12,
      height = 22,
      col = {
        active = "rgba(edb443ff)",
        inactive = "rgba(091f2eff)",
      },
      text_color = "rgba(091f2eff)",
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

hl.animation({ leaf = "global", enabled = true, speed = 8, bezier = "default" })
hl.animation({ leaf = "windows", enabled = true, speed = 6, bezier = "overshoot", style = "gnomed" })
hl.animation({ leaf = "windowsIn", enabled = true, speed = 6, bezier = "overshoot", style = "gnomed" })
hl.animation({ leaf = "windowsOut", enabled = true, speed = 5, bezier = "smoothInOut", style = "gnomed" })
hl.animation({ leaf = "windowsMove", enabled = true, speed = 6, bezier = "smoothOut" })
hl.animation({ leaf = "border", enabled = false })
hl.animation({ leaf = "borderangle", enabled = false })
hl.animation({ leaf = "fade", enabled = true, speed = 5, bezier = "smoothOut" })
hl.animation({ leaf = "workspaces", enabled = true, speed = 6, bezier = "smoothOut", style = "slidefade 15%" })
hl.animation({ leaf = "specialWorkspace", enabled = true, speed = 6, bezier = "smoothOut", style = "slidevert" })

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
end

bind(main_mod .. " + P", exec(menu))
bind(main_mod .. " + SHIFT + P", exec(run_menu))
bind(main_mod .. " + SHIFT + Return", exec(terminal))
bind(main_mod .. " + Q", hl.dsp.window.close())
bind(main_mod .. " + SHIFT + C", hl.dsp.window.close())
bind(main_mod .. " + SHIFT + Q", hl.dsp.exit())
bind(main_mod .. " + E", exec("emacsclient --eval '(emacs-everywhere)'"))
bind(main_mod .. " + V", exec("wl-paste | xdotool type --file -"))
bind(main_mod .. " + Tab", hyprexpo("toggle"))
bind(main_mod .. " + SHIFT + Tab", hyprexpo("bring"))
bind(main_mod .. " + G", function()
  enter_window_picker("go")
end)
bind(main_mod .. " + B", function()
  enter_window_picker("bring")
end)
bind(main_mod .. " + SHIFT + B", function()
  enter_window_picker("replace")
end)

bind(main_mod .. " + W", function()
  focus_direction("up")
end)
bind(main_mod .. " + S", function()
  focus_direction("down")
end)
bind(main_mod .. " + A", function()
  focus_direction("left")
end)
bind(main_mod .. " + D", function()
  focus_direction("right")
end)

bind(main_mod .. " + SHIFT + W", hl.dsp.window.swap({ direction = "up" }))
bind(main_mod .. " + SHIFT + S", hl.dsp.window.swap({ direction = "down" }))
bind(main_mod .. " + SHIFT + A", hl.dsp.window.swap({ direction = "left" }))
bind(main_mod .. " + SHIFT + D", hl.dsp.window.swap({ direction = "right" }))

bind(main_mod .. " + CTRL + W", function()
  move_window_to_monitor("u", false)
end)
bind(main_mod .. " + CTRL + S", function()
  move_window_to_monitor("d", false)
end)
bind(main_mod .. " + CTRL + A", function()
  move_window_to_monitor("l", false)
end)
bind(main_mod .. " + CTRL + D", function()
  move_window_to_monitor("r", false)
end)
bind(main_mod .. " + CTRL + SHIFT + W", function()
  move_window_to_empty_workspace_on_monitor("u")
end)
bind(main_mod .. " + CTRL + SHIFT + S", function()
  move_window_to_empty_workspace_on_monitor("d")
end)
bind(main_mod .. " + CTRL + SHIFT + A", function()
  move_window_to_empty_workspace_on_monitor("l")
end)
bind(main_mod .. " + CTRL + SHIFT + D", function()
  move_window_to_empty_workspace_on_monitor("r")
end)

hl.define_submap("swap-workspace", function()
  for i = 1, 9 do
    local workspace_id = i
    bind(tostring(i), function()
      swap_current_workspace_with(workspace_id)
      hl.dsp.submap("reset")()
    end)
  end

  bind("Escape", hl.dsp.submap("reset"))
  bind("catchall", hl.dsp.submap("reset"))
end)

hl.define_submap("window-picker", function()
  for i = 1, 9 do
    local index = i
    bind(tostring(i), function()
      activate_window_picker_candidate(index)
    end)
  end

  bind("Escape", hl.dsp.submap("reset"))
  bind("catchall", hl.dsp.submap("reset"))
end)

bind(mod_alt .. " + SHIFT + W", hl.dsp.window.resize({ x = 0, y = -50, relative = true }), { repeating = true })
bind(mod_alt .. " + SHIFT + S", hl.dsp.window.resize({ x = 0, y = 50, relative = true }), { repeating = true })
bind(mod_alt .. " + SHIFT + A", hl.dsp.window.resize({ x = -50, y = 0, relative = true }), { repeating = true })
bind(mod_alt .. " + SHIFT + D", hl.dsp.window.resize({ x = 50, y = 0, relative = true }), { repeating = true })

bind(hyper .. " + W", hl.dsp.focus({ monitor = "u" }))
bind(hyper .. " + S", hl.dsp.focus({ monitor = "d" }))
bind(hyper .. " + A", hl.dsp.focus({ monitor = "l" }))
bind(hyper .. " + D", hl.dsp.focus({ monitor = "r" }))
bind(hyper .. " + SHIFT + W", function()
  move_window_to_monitor("u", true)
end)
bind(hyper .. " + SHIFT + S", function()
  move_window_to_monitor("d", true)
end)
bind(hyper .. " + SHIFT + A", function()
  move_window_to_monitor("l", true)
end)
bind(hyper .. " + SHIFT + D", function()
  move_window_to_monitor("r", true)
end)

bind(main_mod .. " + Space", toggle_columns_monocle)
bind(main_mod .. " + SHIFT + Space", function()
  set_layout(columns_layout)
end)
bind(main_mod .. " + CTRL + Space", function()
  set_layout(monocle_layout)
end)
bind(main_mod .. " + bracketright", monocle_next)
bind(main_mod .. " + bracketleft", monocle_prev)
bind(main_mod .. " + F", hl.dsp.window.fullscreen({ mode = "fullscreen" }))
bind(main_mod .. " + SHIFT + F", hl.dsp.window.fullscreen({ mode = "maximized" }))
bind(main_mod .. " + T", hl.dsp.window.float())
bind(main_mod .. " + M", minimize_active_window)
bind(main_mod .. " + SHIFT + M", restore_last_minimized)
bind(main_mod .. " + CTRL + SHIFT + M", function()
  enter_window_picker("minimized")
end)
bind(main_mod .. " + SHIFT + equal", schedule_nstack_count_update)
bind(main_mod .. " + CTRL + M", hl.dsp.window.toggle_swallow())
bind(main_mod .. " + SHIFT + E", function()
  move_to_next_empty_workspace(true)
end)
bind(main_mod .. " + CTRL + E", function()
  move_to_next_empty_workspace(false)
end)
bind(main_mod .. " + apostrophe", focus_next_class)
bind(mod_alt .. " + W", show_active_window_info)

bind(main_mod .. " + X", hl.dsp.window.move({ workspace = "special:NSP" }))
bind(main_mod .. " + SHIFT + X", hl.dsp.workspace.toggle_special("NSP"))
bind(mod_alt .. " + E", function()
  toggle_scratchpad("element")
end)
bind(mod_alt .. " + G", function()
  toggle_scratchpad("gmail")
end)
bind(mod_alt .. " + H", function()
  toggle_scratchpad("htop")
end)
bind(mod_alt .. " + M", function()
  toggle_scratchpad("messages")
end)
bind(mod_alt .. " + K", function()
  toggle_scratchpad("slack")
end)
bind(mod_alt .. " + S", function()
  toggle_scratchpad("spotify")
end)
bind(mod_alt .. " + T", function()
  toggle_scratchpad("transmission")
end)
bind(mod_alt .. " + V", function()
  toggle_scratchpad("volume")
end)
bind(mod_alt .. " + grave", function()
  toggle_scratchpad("dropdown")
end)
bind(mod_alt .. " + C", function()
  raise_or_spawn("google-chrome", "google-chrome-stable")
end)
bind(mod_alt .. " + Space", minimize_other_classes)
bind(mod_alt .. " + SHIFT + Space", restore_focused_class)
bind(mod_alt .. " + Return", restore_all_minimized)

for i = 1, 9 do
  local workspace = tostring(i)
  bind(main_mod .. " + " .. workspace, hl.dsp.focus({ workspace = workspace, on_current_monitor = true }))
  bind(main_mod .. " + SHIFT + " .. workspace, hl.dsp.window.move({ workspace = workspace, follow = false }))
  bind(main_mod .. " + CTRL + " .. workspace, function()
    hl.dsp.window.move({ workspace = workspace, follow = false })()
    hl.dsp.focus({ workspace = workspace, on_current_monitor = true })()
  end)
end

bind(main_mod .. " + backslash", focus_previous_workspace_for_monitor)
bind(main_mod .. " + Z", hl.dsp.focus({ monitor = "+1" }))
bind(main_mod .. " + SHIFT + Z", hl.dsp.window.move({ monitor = "+1" }))
bind(main_mod .. " + mouse_down", function()
  cycle_workspace(1)
end)
bind(main_mod .. " + mouse_up", function()
  cycle_workspace(-1)
end)
bind(hyper .. " + E", focus_next_empty_workspace)
bind(hyper .. " + 5", enter_workspace_swap_mode)
bind(hyper .. " + G", gather_focused_class)

bind(main_mod .. " + I", exec("set_volume --unmute --change-volume +5"), { repeating = true })
bind(main_mod .. " + K", exec("set_volume --unmute --change-volume -5"), { repeating = true })
bind(main_mod .. " + U", exec("set_volume --toggle-mute"))
bind(main_mod .. " + semicolon", exec("playerctl play-pause"))
bind(main_mod .. " + L", exec("playerctl next"))
bind(main_mod .. " + J", exec("playerctl previous"))

bind("XF86AudioPlay", exec("playerctl play-pause"))
bind("XF86AudioPause", exec("playerctl play-pause"))
bind("XF86AudioNext", exec("playerctl next"))
bind("XF86AudioPrev", exec("playerctl previous"))
bind("XF86AudioRaiseVolume", exec("set_volume --unmute --change-volume +5"), { repeating = true })
bind("XF86AudioLowerVolume", exec("set_volume --unmute --change-volume -5"), { repeating = true })
bind("XF86AudioMute", exec("set_volume --toggle-mute"))
bind("XF86MonBrightnessUp", exec("brightness.sh up"), { repeating = true })
bind("XF86MonBrightnessDown", exec("brightness.sh down"), { repeating = true })

bind(hyper .. " + V", exec([[cliphist list | rofi -dmenu -p "Clipboard" | cliphist decode | wl-copy]]))
bind(hyper .. " + P", exec("rofi-pass"))
bind(hyper .. " + H", exec([[grim -g "$(slurp)" - | swappy -f -]]))
bind(hyper .. " + C", exec("shell_command.sh"))
bind(hyper .. " + X", exec("rofi_command.sh"))
bind(hyper .. " + SHIFT + L", exec("hyprlock"))
bind(hyper .. " + K", exec("rofi_kill_process.sh"))
bind(hyper .. " + SHIFT + K", exec("rofi_kill_all.sh"))
bind(hyper .. " + R", exec("rofi-systemd"))
bind(hyper .. " + slash", exec("toggle_taffybar"))
bind(hyper .. " + I", exec("rofi_select_input.hs"))
bind(hyper .. " + backslash", exec("/home/imalison/dotfiles/dotfiles/lib/functions/mpg341cx_input toggle"))
bind(hyper .. " + O", exec("rofi_paswitch"))
bind(hyper .. " + comma", exec("rofi_wallpaper.sh"))
bind(hyper .. " + Y", exec("rofi_agentic_skill"))
bind(main_mod .. " + R", exec("hyprctl reload"))

bind(main_mod .. " + mouse:272", hl.dsp.window.drag())
bind(main_mod .. " + mouse:273", hl.dsp.window.resize())

hl.on("hyprland.start", function()
  apply_nstack_config()
  apply_hyprexpo_config()
  apply_rules()
  hl.exec_cmd("sh -lc 'export IMALISON_SESSION_TYPE=wayland; dbus-update-activation-environment --systemd WAYLAND_DISPLAY DISPLAY XAUTHORITY HYPRLAND_INSTANCE_SIGNATURE XDG_CURRENT_DESKTOP XDG_SESSION_TYPE IMALISON_SESSION_TYPE; systemctl --user start graphical-session.target hyprland-session.target'")
  hl.exec_cmd("hypridle")
  hl.exec_cmd("wl-paste --type text --watch cliphist store")
  hl.exec_cmd("wl-paste --type image --watch cliphist store")
  remember_workspace_for_monitor()
  write_layout_state()
  schedule_nstack_count_update()
end)

hl.on("config.reloaded", apply_nstack_config)
hl.on("config.reloaded", apply_hyprexpo_config)
hl.on("config.reloaded", apply_rules)

hl.on("window.open", schedule_nstack_count_update)
hl.on("window.destroy", schedule_nstack_count_update)
hl.on("window.kill", schedule_nstack_count_update)
hl.on("window.move_to_workspace", schedule_nstack_count_update)
hl.on("workspace.active", sync_layout_for_active_workspace)
hl.on("monitor.focused", sync_layout_for_active_workspace)
hl.on("workspace.active", remember_workspace_for_monitor)

hl.on("window.open", update_monocle_notice)
hl.on("window.destroy", update_monocle_notice)
hl.on("window.kill", update_monocle_notice)
hl.on("window.move_to_workspace", update_monocle_notice)

hl.on("window.open", adopt_matching_scratchpad_window)
hl.on("window.class", adopt_matching_scratchpad_window)
hl.on("window.title", adopt_matching_scratchpad_window)
