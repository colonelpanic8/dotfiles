local root = assert(arg[1])
local runtime = assert(os.getenv("XDG_RUNTIME_DIR"))
local signature = assert(os.getenv("HYPRLAND_INSTANCE_SIGNATURE"))
local state_path = runtime .. "/hyprland-layouts-" .. signature
local workspaces = {
  [1] = { id = 1, tiled_layout = "nStack" },
  [2] = { id = 2, tiled_layout = "master" },
}
local active = workspaces[1]
local plugin = true
local rules, notices, flushes = {}, {}, 0
local ctx
local function load()
  rules = {}
  hl = {
    get_loaded_plugins = function() return plugin and {{ name = "hyprfocus" }} or {} end,
    notification = { create = function(item) notices[#notices + 1] = item.text end },
    workspace_rule = function(rule)
      for _, handle in ipairs(rules) do
        if handle.enabled and handle.selector == rule.workspace then
          handle.layout = rule.layout
          return handle
        end
      end
      local handle = { selector = rule.workspace, layout = rule.layout, enabled = true }
      function handle:set_enabled(enabled) self.enabled = enabled end
      rules[#rules + 1] = handle
      return handle
    end,
    exec_scheduled_prop_refresh_immediately = function()
      flushes = flushes + 1
      for id, workspace in pairs(workspaces) do
        local layout = id == 2 and "master" or "nStack"
        for _, rule in ipairs(rules) do
          if rule.enabled and rule.selector == "r[" .. id .. "-" .. id .. "]s[false]" then
            layout = rule.layout
          end
        end
        workspace.tiled_layout = layout
        ctx.workspace_layout_changed(workspace)
      end
    end,
  }
  ctx = setmetatable({
    tabbed_layout = "tabbed", columns_layout = "nStack", workspace_layouts = {},
    layout_names = { nStack = "Columns", master = "Main", tabbed = "Focus", grid = "Grid" },
    notification_icons = {warning = 0},
    workspace_key = function(workspace) return tostring((workspace or active).id) end,
    active_workspace = function() return active end,
    is_normal_workspace = function(workspace) return workspace and workspace.id > 0 and not workspace.special end,
    hyprland_layout = function(layout) return layout == "grid" and "nStack" or layout end,
    write_layout_state = function() end,
  }, { __index = _G })
  dofile(root .. "/hyprland/layout_policy.lua").setup(ctx)
end

os.remove(state_path)
load()
assert(ctx.apply_workspace_layout(workspaces[1], "tabbed"))
assert(workspaces[1].tiled_layout == "tabbed" and workspaces[2].tiled_layout == "master")
assert(ctx.focus_return_layout(workspaces[1]) == "nStack")
active = workspaces[2]
assert(ctx.workspace_layout(workspaces[1]) == "tabbed")
assert(ctx.workspace_layout(active) == "master")
assert(ctx.apply_workspace_layout(active, "tabbed"))
assert(ctx.focus_return_layout(active) == "master")
assert(ctx.apply_workspace_layout(active, ctx.focus_return_layout(active)))
assert(active.tiled_layout == "master")

-- Replay queues rules without rebuilding partially loaded workspace policy.
local before = flushes
load()
ctx.restore_layout_policy()
assert(flushes == before)
assert(ctx.workspace_layouts["1"] == "tabbed" and ctx.workspace_layouts["2"] == "master")
hl.exec_scheduled_prop_refresh_immediately()
for _ = 1, 20 do
  ctx.forget_workspace_layout(workspaces[1])
  workspaces[1].tiled_layout = "nStack"
  assert(ctx.apply_workspace_layout(workspaces[1], "tabbed"))
end
assert(#rules == 2, "workspace reuse must not leak rules")
assert(not pcall(hl.workspace_rule, {workspace = "r[1-1]s[false]", layout = "master"}))
assert(pcall(hl.workspace_rule, {workspace = "r[1-9]s[false]", layout = "master"}))

-- An unavailable plugin cannot change either the layout or the remembered mode.
plugin = false
assert(not ctx.apply_workspace_layout(workspaces[2], "tabbed"))
assert(workspaces[2].tiled_layout == "master")
workspaces[1].tiled_layout = "nStack"
ctx.workspace_layout_changed(workspaces[1])
assert(ctx.workspace_layouts["1"] == nil)
load()
ctx.restore_layout_policy()
assert(ctx.workspace_layouts["1"] == nil, "plugin fallback must be persisted")

local file = assert(io.open(state_path, "w"))
file:write("hyprland-layouts-v1\n1\ttabbed\tnStack\n2\tbad\t-\n")
file:close()
load()
ctx.restore_layout_policy()
assert(next(ctx.workspace_layouts) == nil, "reject malformed policy as a whole")
assert(#rules == 0)
os.remove(state_path)
print("workspace layout policy tests passed")
