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
const remoteMinimized = { id: null, name: "special:minimized", monitor: second };
const special = { id: null, name: "special:NSP", monitor };
const scratchpad = { id: -1, name: "scratch-hidden-codex", address: "scratch-hidden-codex", monitor };
const windows = [occupied, minimized, remoteMinimized, special, scratchpad].map(workspace => ({ workspace, wayland: {} }));
const raw = {
    workspaces: [occupied, empty, other, minimized, remoteMinimized, special, scratchpad],
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
assert.equal(model.hyprlandWorkspaceMatches(remoteMinimized, bucket), false);
assert.equal(model.hyprlandWorkspaceMatches(special, bucket), false);
assert.equal(model.hyprlandWorkspaceMatches(scratchpad, bucket), false);
assert.equal(model.hyprlandWorkspaceMatches(occupied, bucket), false);
assert.deepEqual(Array.from(model.hyprlandWindowsOnWorkspace(windows.map(w => w.wayland), bucket, windows)), [windows[1].wayland]);
minimized.name = "minimized";
assert.equal(model.hyprlandWorkspaceMatches(minimized, bucket), true);
assert.equal(model.hyprlandSpecial({ id: -1, name: "project", address: "0xabc" }), false);
raw.toplevels = windows.filter(w => w.workspace !== minimized && w.workspace !== remoteMinimized);
assert.deepEqual(list(monitor.name).map(ws => ws.name), ["work"]);
raw.toplevels = [];
assert.deepEqual(list(monitor.name).map(ws => ws.name), ["work"]);
console.log("Workspace behavior: filtering, monitor ownership, hidden scratchpads, minimized bucket, empty bucket removal passed.");
