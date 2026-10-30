const assert = require("node:assert/strict");
const fs = require("node:fs");
const vm = require("node:vm");

const model = vm.createContext({});
vm.runInContext(fs.readFileSync(process.argv[2], "utf8").replace(/^\.pragma.*\n/gm, ""), model);
const monitor = { name: "DP-1" };
const second = { name: "DP-2" };
const occupied = { id: 1, name: "work", monitor, active: true };
const empty = { id: 2, name: "empty", monitor };
const other = { id: 3, name: "elsewhere", monitor: second };
const minimized = { id: -99, name: "special:minimized", monitor };
const scratchpad = { id: null, name: "special:terminal", monitor };
const remoteScratchpad = { id: null, name: "special:music", monitor: second };
const windows = [occupied, minimized, scratchpad, remoteScratchpad].map(workspace => ({ workspace, wayland: {} }));
const raw = {
    workspaces: [occupied, empty, other, minimized, scratchpad, remoteScratchpad],
    monitors: [{ name: monitor.name, activeWorkspace: occupied }, { name: second.name, activeWorkspace: other }],
    focusedWorkspace: occupied,
    toplevels: windows,
    workspaceRules: []
};
const list = (screen, follow = false, special = true) => Array.from(model.hyprlandWorkspacesForScreen(raw, screen, follow, true, 0, special));
assert.deepEqual(list(monitor.name).map(ws => ws.name), ["work", "M"]);
assert.deepEqual(list(second.name).map(ws => ws.name), ["elsewhere", "M"]);
assert.deepEqual(list(monitor.name, true).map(ws => ws.name), ["work", "M"]);
assert.deepEqual(list(monitor.name, false, false).map(ws => ws.name), ["work"]);
const bucket = list(monitor.name).find(ws => ws.minimizedBucket);
assert.equal(model.hyprlandWorkspaceMatches(minimized, bucket), true);
assert.equal(model.hyprlandWorkspaceMatches(scratchpad, bucket), true);
assert.equal(model.hyprlandWorkspaceMatches(remoteScratchpad, bucket), false);
assert.equal(model.hyprlandWorkspaceMatches(occupied, bucket), false);
assert.deepEqual(Array.from(model.hyprlandWindowsOnWorkspace(windows.map(w => w.wayland), bucket, windows)), [windows[1].wayland, windows[2].wayland]);
minimized.name = "minimized";
scratchpad.name = "scratch-hidden-claude";
scratchpad.id = -1377;
assert.deepEqual(list(monitor.name).map(ws => ws.name), ["work", "M"]);
assert.equal(model.hyprlandWorkspaceMatches(scratchpad, bucket), true);
assert.equal(model.hyprlandSpecial({ id: -1, name: "project", address: "0xabc" }), false);
raw.toplevels = [];
assert.deepEqual(list(monitor.name).map(ws => ws.name), ["work"]);
console.log("Workspace behavior: filtering, monitor ownership, scratchpad aggregation, empty bucket removal passed.");
