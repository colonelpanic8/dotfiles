local M = {}

function M.setup(ctx)
  local _ENV = ctx
  local rules = {}
  local returns = {}
  local applying = false
  local runtime = os.getenv("XDG_RUNTIME_DIR")
  local signature = os.getenv("HYPRLAND_INSTANCE_SIGNATURE")
  local path = runtime and signature and signature:match("^[%w_-]+$")
    and (runtime .. "/hyprland-layouts-" .. signature) or nil
  local workspace_rule = hl.workspace_rule

  hl.workspace_rule = function(rule)
    if type(rule.workspace) == "string" then
      local first, last = rule.workspace:match("^r%[(%d+)%-(%d+)%]s%[false%]$")
      if first and first == last then
        error("Workspace selector reserved for layout policy: " .. rule.workspace)
      end
    end
    return workspace_rule(rule)
  end

  local function report(message)
    hl.notification.create({ text = message, duration = 3000, icon = notification_icons.warning })
  end

  local function available(layout)
    if layout ~= tabbed_layout then
      return true
    end
    for _, plugin in ipairs(hl.get_loaded_plugins()) do
      if plugin.name == "hyprfocus" then
        return true
      end
    end
    return false
  end

  local function save()
    if not path or verify_config then
      return
    end
    local file = io.open(path .. ".tmp", "w")
    if not file then
      report("Unable to save workspace layouts")
      return
    end
    file:write("hyprland-layouts-v1\n")
    for key, layout in pairs(workspace_layouts) do
      file:write(key, "\t", layout, "\t", returns[key] or "-", "\n")
    end
    local ok = file:close()
    if not ok or not os.rename(path .. ".tmp", path) then
      report("Unable to save workspace layouts")
    end
  end

  local function apply(key, layout)
    if rules[key] then
      rules[key]:set_enabled(true)
    end
    rules[key] = workspace_rule({ workspace = "r[" .. key .. "-" .. key .. "]s[false]", layout = hyprland_layout(layout) })
  end

  local function actual(workspace)
    local configured = workspace_layouts[workspace_key(workspace)]
    local live = workspace and workspace.tiled_layout
    if configured and live and hyprland_layout(configured):lower() == live:lower() then
      return configured
    end
    for layout in pairs(layout_names) do
      if live and layout:lower() == live:lower() then
        return layout
      end
    end
    return live or columns_layout
  end

  ctx.workspace_layout = actual
  ctx.focus_return_layout = function(workspace)
    return returns[workspace_key(workspace)] or columns_layout
  end

  ctx.apply_workspace_layout = function(workspace, layout)
    if not is_normal_workspace(workspace) then
      report("Layout selection requires a numbered workspace")
      return false
    end
    if not available(layout) then
      report("Focus mode requires the hyprfocus plugin")
      return false
    end
    local key = workspace_key(workspace)
    local previous = actual(workspace)
    applying = true
    apply(key, layout)
    hl.exec_scheduled_prop_refresh_immediately()
    applying = false
    if not workspace.tiled_layout or workspace.tiled_layout:lower() ~= hyprland_layout(layout):lower() then
      apply(key, previous)
      hl.exec_scheduled_prop_refresh_immediately()
      report("Unable to activate layout: " .. layout)
      return false
    end
    if layout == tabbed_layout then
      if previous ~= tabbed_layout then
        returns[key] = previous
      end
    else
      returns[key] = nil
    end
    workspace_layouts[key] = layout
    save()
    return true
  end

  ctx.restore_layout_policy = function()
    if not path or verify_config then
      return
    end
    local file = io.open(path, "r")
    if not file then
      return
    end
    local contents = file:read(65537)
    file:close()
    local records = {}
    local valid = contents and #contents <= 65536 and contents:sub(1, 20) == "hyprland-layouts-v1\n"
    if valid then
      for line in contents:sub(21):gmatch("[^\n]+") do
        local key, layout, back = line:match("^(%d+)\t([%w]+)\t([%w%-]+)$")
        if not key or tonumber(key) < 1 or tonumber(key) > 2147483647 or tostring(tonumber(key)) ~= key
          or records[key] or not layout_names[layout]
          or (layout == tabbed_layout and (not layout_names[back] or back == tabbed_layout))
          or (layout ~= tabbed_layout and back ~= "-") then
          valid = false
          break
        end
        records[key] = { layout = layout, back = back }
      end
    end
    if not valid then
      report("Ignoring malformed workspace layout state")
      return
    end
    for key, record in pairs(records) do
      if available(record.layout) then
        workspace_layouts[key] = record.layout
        returns[key] = record.back ~= "-" and record.back or nil
        apply(key, record.layout)
      else
        report("Focus mode unavailable; using the default layout")
      end
    end
    save()
  end

  ctx.forget_workspace_layout = function(workspace)
    if not is_normal_workspace(workspace) then
      return
    end
    local key = workspace_key(workspace)
    if rules[key] then
      rules[key]:set_enabled(false)
    end
    workspace_layouts[key] = nil
    returns[key] = nil
    save()
  end

  ctx.workspace_layout_changed = function(workspace)
    if applying or not is_normal_workspace(workspace) then
      return
    end
    local key = workspace_key(workspace)
    local desired = workspace_layouts[key]
    if desired and actual(workspace) ~= desired then
      workspace_layouts[key] = nil
      returns[key] = nil
      if rules[key] then
        rules[key]:set_enabled(false)
      end
      save()
      report("Workspace " .. key .. " now uses " .. actual(workspace))
    end
    current_layout = actual(active_workspace())
    write_layout_state()
  end
end

return M
