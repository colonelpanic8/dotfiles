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
const scratchpad = { id: null, name: "special:scratch-codex", monitor };
const windows = [occupied, minimized, remoteMinimized, scratchpad].map(workspace => ({ workspace, wayland: {} }));
const raw = {
    workspaces: [occupied, empty, other, minimized, remoteMinimized, scratchpad],
    monitors: [{ name: monitor.name, activeWorkspace: occupied }, { name: second.name, activeWorkspace: other }],
    focusedWorkspace: occupied,
    toplevels: windows,
    workspaceRules: []
};
const list = (screen, follow = false, special = true) => Array.from(model.hyprlandWorkspacesForScreen(raw, screen, follow, true, 0, special));
assert.deepEqual(list(monitor.name).map(ws => ws.name), ["work", "M", "S"]);
assert.deepEqual(list(second.name).map(ws => ws.name), ["elsewhere", "M", "S"]);
assert.deepEqual(list(monitor.name, true).map(ws => ws.name), ["work", "M", "S"]);
assert.deepEqual(list(monitor.name, false, false).map(ws => ws.name), ["work"]);
const bucket = list(monitor.name).find(ws => ws.bucket === "minimized");
const scratchBucket = list(monitor.name).find(ws => ws.bucket === "scratchpad");
assert.equal(model.hyprlandWorkspaceMatches(minimized, bucket), true);
assert.equal(model.hyprlandWorkspaceMatches(remoteMinimized, bucket), false);
assert.equal(model.hyprlandWorkspaceMatches(scratchpad, bucket), false);
assert.equal(model.hyprlandWorkspaceMatches(occupied, bucket), false);
assert.equal(model.hyprlandWorkspaceMatches(scratchpad, scratchBucket), true);
assert.equal(model.hyprlandWorkspaceMatches(minimized, scratchBucket), false);
assert.equal(model.hyprlandWorkspaceMatches({ id: null, name: "special:NSP", monitor: second }, scratchBucket), true);
assert.deepEqual(Array.from(model.hyprlandWindowsOnWorkspace(windows.map(w => w.wayland), bucket, windows)), [windows[1].wayland]);
minimized.name = "minimized";
assert.equal(model.hyprlandWorkspaceMatches(minimized, bucket), true);
minimized.name = "special:minimized";
assert.equal(model.hyprlandSpecial({ id: -1, name: "project", address: "0xabc" }), false);
raw.toplevels = windows.filter(w => w.workspace !== minimized && w.workspace !== remoteMinimized);
assert.deepEqual(list(monitor.name).map(ws => ws.name), ["work", "S"]);
raw.toplevels = windows.filter(w => w.workspace !== scratchpad);
assert.deepEqual(list(monitor.name).map(ws => ws.name), ["work", "M"]);
raw.toplevels = [];
assert.deepEqual(list(monitor.name).map(ws => ws.name), ["work"]);
console.log("Workspace behavior: filtering, monitor ownership, minimized and scratchpad buckets, empty bucket removal passed.");
