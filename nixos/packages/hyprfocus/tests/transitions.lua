local function run(dispatcher)
    local result = hl.dispatch(dispatcher)
    assert(result.ok, result.error)
end
local function selector(window) return "address:" .. window.address end
local function layout(id, name)
    assert(focus_test_ctx.apply_workspace_layout(hl.get_workspace(tostring(id)), name))
    assert(hl.get_workspace(tostring(id)).tiled_layout == name)
end
local function invariant(id, count)
    local visible, windows = 0, 0
    for _, window in ipairs(hl.get_workspace_windows(hl.get_workspace(tostring(id)))) do
        if not window.floating then
            windows = windows + 1
            assert(not window.group, "tiled group on focus workspace")
            if window.visible then
                visible = visible + 1
                assert(window.accepts_input)
            else
                assert(not window.accepts_input)
            end
        end
    end
    assert(windows == count, "wrong membership: " .. windows .. " expected " .. count)
    assert(visible == (count > 0 and 1 or 0), "wrong visibility: " .. visible)
end
local windows = {}
for _, window in ipairs(hl.get_windows()) do
    assert(window.title:match("^focus%-test%-"), "test compositor contains unrelated clients")
    windows[window.title:sub(-1)] = window
end
local a, b, c = assert(windows.a), assert(windows.b), assert(windows.c)
local function focus(window) run(hl.dsp.focus({window = selector(window)})) end
local function move(window, id, follow) run(hl.dsp.window.move({window = selector(window), workspace = tostring(id), follow = follow or false})) end

layout(1, "dwindle")
layout(2, "dwindle")
move(a, 2)
move(b, 2)
move(c, 2)
move(a, 1)
move(b, 1)
move(c, 1)
layout(1, "tabbed")
invariant(1, 3)
focus(a)
assert(a.visible and not b.visible)
run(hl.dsp.layout("cyclenext"))
assert(b.visible and not a.visible)
run(hl.dsp.layout("movetab 1"))
run(hl.dsp.layout("cycleprev"))
assert(c.visible)
run(hl.dsp.layout("focustab 1"))
assert(a.visible)
local refused = hl.dispatch(hl.dsp.group.toggle({window = selector(a)}))
assert(not refused.ok)
invariant(1, 3)
move(b, 2)
invariant(1, 2)
assert(hl.get_workspace("2").tiled_layout == "dwindle")
move(b, 1)
invariant(1, 3)
assert(a.visible, "silent arrival stole selection")
focus(b)
assert(b.visible)
run(hl.dsp.window.float({window = selector(b), action = "enable"}))
invariant(1, 2)
assert(b.floating and b.active)
run(hl.dsp.window.swap({window = selector(b), target = selector(a)}))
assert(a.floating and not b.floating)
invariant(1, 2)
run(hl.dsp.window.swap({window = selector(a), target = selector(b)}))
assert(b.floating and not a.floating)
invariant(1, 2)
focus(b)
run(hl.dsp.window.float({window = selector(b), action = "disable"}))
invariant(1, 3)
assert(b.visible)
move(b, 2)
move(c, 2)
invariant(1, 1)
assert(a.at.y == 21, "single tab kept strip reservation")
focus(b)
run(hl.dsp.group.toggle({window = selector(b)}))
b.group:add(c)
assert(b.group.size == 2)
local before = {b.workspace.id, b.floating, b.fullscreen}
refused = hl.dispatch(hl.dsp.window.move({window = selector(b), workspace = "1", follow = false}))
assert(not refused.ok and b.workspace.id == before[1] and b.floating == before[2] and b.fullscreen == before[3])
assert(b.group.size == 2)
refused = hl.dispatch(hl.dsp.window.swap({window = selector(b), target = selector(a)}))
assert(not refused.ok and b.workspace.id == 2 and a.workspace.id == 1)
invariant(1, 1)
-- Floating groups remain usable, but cannot become tiled in focus mode.
run(hl.dsp.window.float({window = selector(b), action = "enable"}))
move(b, 1)
assert(b.floating and c.floating)
refused = hl.dispatch(hl.dsp.window.float({window = selector(b), action = "disable"}))
assert(not refused.ok and b.floating and b.group.size == 2)
invariant(1, 1)
move(b, 2)
run(hl.dsp.window.float({window = selector(b), action = "disable"}))
focus(b)
run(hl.dsp.window.fullscreen_state({window = selector(b), internal = 2, client = 2, action = "set"}))
-- Entry normalises existing groups before the next dispatch in this same callback.
layout(2, "tabbed")
assert(b.fullscreen == 2 and b.fullscreen_client == 2)
run(hl.dsp.window.fullscreen_state({window = selector(b), internal = 2, client = 2, action = "unset"}))
invariant(2, 2)
assert(b.visible)
refused = hl.dispatch(hl.dsp.group.toggle({window = selector(b)}))
assert(not refused.ok)
invariant(2, 2)
move(c, 1)
invariant(2, 1)
invariant(1, 2)
assert(a.visible)
focus(b)
move(b, 1)
assert(a.visible, "silently moving the focused window stole destination selection")
invariant(1, 3)
invariant(2, 0)
move(b, 2, true)
move(b, 1, true)
assert(b.visible and b.active, "bring-and-focus did not select arrival")
move(b, 2)
focus(c)
assert(c.visible)
layout(1, "dwindle")
assert(a.visible and c.visible and a.accepts_input and c.accepts_input)
layout(1, "tabbed")
invariant(1, 2)
_G.focus_test_invariant = invariant
_G.focus_test_passed = true
