hl.monitor({output = "HEADLESS-1", mode = "1280x720@60", position = "0x0", scale = 1})
hl.monitor({output = "HEADLESS-2", mode = "1280x720@60", position = "1280x0", scale = 1})
hl.monitor({output = "", mode = "preferred", position = "auto", scale = 1})
hl.config({
    general = {layout = "dwindle", gaps_in = 5, gaps_out = 20, border_size = 1},
    animations = {enabled = false},
    ecosystem = {no_update_news = true, no_donation_nag = true},
    misc = {disable_hyprland_logo = true, disable_splash_rendering = true},
    debug = {disable_logs = false},
})
hl.plugin.load(assert(os.getenv("HYPRFOCUS_PLUGIN")))
_G.hyprfocus_test_session = true
local ctx = setmetatable({
    tabbed_layout = "tabbed", columns_layout = "dwindle", workspace_layouts = {},
    layout_names = {dwindle = "Dwindle", master = "Master", tabbed = "Focus"}, notification_icons = {warning = 0},
    workspace_key = function(ws) return tostring((ws or hl.get_active_workspace()).id) end,
    active_workspace = hl.get_active_workspace,
    is_normal_workspace = function(ws) return ws and not ws.special and ws.id and ws.id > 0 end,
    hyprland_layout = function(name) return name end,
    write_layout_state = function() end,
}, {__index = _G})
dofile(assert(os.getenv("HYPRFOCUS_POLICY"))).setup(ctx)
hl.on("workspace.layout_changed", ctx.workspace_layout_changed)
hl.on("workspace.destroyed", ctx.forget_workspace_layout)
ctx.restore_layout_policy()
_G.focus_test_ctx = ctx
