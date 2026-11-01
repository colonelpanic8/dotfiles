#!/usr/bin/env python3
"""Exercise focus mode inside a private compositor with an isolated Wayland parent."""
import argparse
import json
import os
from pathlib import Path
import subprocess
import tempfile
import time

parser = argparse.ArgumentParser(description=__doc__)
for option in ["compositor", "hyprctl", "plugin", "parent-wayland"]:
    parser.add_argument("--" + option, required=True)
parser.add_argument("--wayland-compat", help="optional test-only nested protocol shim")
parser.add_argument("--wlrctl", help="also exercise tab clicks")
parser.add_argument("--overview-plugin")
args = parser.parse_args()
here = Path(__file__).resolve().parent
runtime = Path(tempfile.mkdtemp(prefix="hf-"))
print(f"Test logs: {runtime}", flush=True)
env = dict(os.environ, XDG_RUNTIME_DIR=str(runtime), WAYLAND_DISPLAY=args.parent_wayland,
           AQ_DRM_DEVICES="/dev/null", HYPRFOCUS_PLUGIN=str(Path(args.plugin).resolve()),
           HYPRFOCUS_POLICY=str(here.parents[3] / "dotfiles/config/hypr/hyprland/layout_policy.lua"))
for name in ["DISPLAY", "HYPRLAND_INSTANCE_SIGNATURE", "LD_PRELOAD"]:
    env.pop(name, None)
if args.wayland_compat:
    env["LD_PRELOAD"] = args.wayland_compat
log = open(runtime / "compositor.log", "w")
compositor = subprocess.Popen([args.compositor, "--config", str(here / "config.lua")], env=env, stdout=log, stderr=subprocess.STDOUT)
clients = []
instance = None


def wait_for(predicate, message):
    deadline = time.monotonic() + 15
    while time.monotonic() < deadline:
        assert compositor.poll() is None, f"compositor exited: {compositor.returncode}"
        if predicate():
            return
        time.sleep(.05)
    raise AssertionError(message)


def ctl(*commands):
    command = [args.hyprctl] + (["-i", instance] if instance else []) + list(commands)
    result = subprocess.run(command, env=env, text=True, capture_output=True, timeout=10)
    assert result.returncode == 0 and "error:" not in result.stdout, result.stdout + result.stderr
    return result.stdout


def evaluate(code):
    return ctl("eval", code)


def windows():
    return json.loads(ctl("-j", "clients"))


def launch(name):
    client_log = open(runtime / f"client-{name}.log", "w")
    process = subprocess.Popen(["ghostty", "--gtk-single-instance=false", f"--title=focus-test-{name}", "-e", "sleep", "300"],
                               env=client_env, stdout=client_log, stderr=subprocess.STDOUT)
    clients.append((process, client_log))
    wait_for(lambda: any(w["title"] == f"focus-test-{name}" for w in windows()), f"client {name} did not map")
    return process


def selected(workspace=1):
    return [w["title"] for w in windows() if w["workspace"]["id"] == workspace and not w["floating"] and w["visible"]]


def focus_workspace(number):
    evaluate(f'assert(hl.dispatch(hl.dsp.focus({{workspace="{number}"}})).ok)')


def passed(message):
    print("PASS " + message, flush=True)


try:
    wait_for(lambda: any(runtime.glob("hypr/*/.socket.sock")), "IPC socket did not appear")
    instances = json.loads(ctl("instances", "-j"))
    instance = instances[0]["instance"]
    client_env = dict(env, WAYLAND_DISPLAY=instances[0]["wl_socket"], GDK_BACKEND="wayland")
    client_env.pop("LD_PRELOAD", None)
    evaluate("assert(hyprfocus_test_session)")
    for number in [1, 2]:
        ctl("output", "create", "headless", f"HEADLESS-{number}")
    for name in "abc":
        launch(name)
    evaluate(f'dofile({json.dumps(str(here / "transitions.lua"))})')
    passed("primitive transitions, groups, moves, floats, fullscreen, selection")

    focus_workspace(1)
    evaluate('assert(hl.dispatch(hl.dsp.layout("focustab 1")).ok)')
    first = selected()
    evaluate('assert(hl.dispatch(hl.dsp.layout("cyclenext")).ok)')
    second = selected()
    ctl("reload")
    assert selected() == second
    evaluate('assert(hl.dispatch(hl.dsp.layout("cyclenext")).ok)')
    assert selected() == first
    evaluate('assert(hl.get_workspace("2").tiled_layout == "tabbed")')
    passed("reload preserves selection, order, and both workspace modes")

    for _ in range(3):
        focus_workspace(3)
        evaluate('assert(focus_test_ctx.workspace_layouts["3"] == nil); assert(focus_test_ctx.apply_workspace_layout(hl.get_workspace("3"), "tabbed"))')
        focus_workspace(1)
        wait_for(lambda: all(w["id"] != 3 for w in json.loads(ctl("-j", "workspaces"))), "empty workspace survived")
        evaluate('assert(focus_test_ctx.workspace_layouts["3"] == nil)')
    passed("destroyed workspace loses policy before its ID is reused")

    evaluate('_G.rule = hl.window_rule({match={title="^focus-test-rule$"}, group="set", no_initial_focus=true})')
    before = selected()
    special = launch("rule")
    assert selected() == before
    window = next(w for w in windows() if w["title"] == "focus-test-rule")
    assert not window["grouped"] and not window["floating"] and not window["acceptsInput"]
    if args.wlrctl:
        evaluate('assert(hl.dispatch(hl.dsp.cursor.move({x=1100,y=25})).ok)')
        subprocess.run([args.wlrctl, "pointer", "click", "left"], env=client_env, check=True, timeout=5)
        wait_for(lambda: selected() == ["focus-test-rule"], "tab click did not select its window")
    special.terminate()
    wait_for(lambda: len(windows()) == 3, "rule test client did not close")
    passed("no-initial-focus map with group rule; pointer input when requested")

    ctl("--batch", 'eval assert(hl.dispatch(hl.dsp.group.toggle({})).ok == false); '
        'eval for _,w in ipairs(hl.get_workspace_windows(hl.get_workspace("1"))) do assert(w.floating or not w.group) end; '
        'eval assert(hl.dispatch(hl.dsp.layout("cyclenext")).ok)')
    passed("IPC batch")
    evaluate('local rule = hl.workspace_rule({workspace="w[tv1]s[false]", gaps_out=0, gaps_in=0}); hl.exec_scheduled_prop_refresh_immediately(); for _,w in ipairs(hl.get_workspace_windows(hl.get_workspace("1"))) do assert(w.at.x==1 and w.at.y==21) end; rule:set_enabled(false); hl.exec_scheduled_prop_refresh_immediately()')
    passed("visible-window gaps rule")

    if args.overview_plugin:
        ctl("plugin", "load", args.overview_plugin)
        evaluate('hl.plugin.hyprwinview.overview("toggle")')
        evaluate('hl.plugin.hyprwinview.overview("toggle")')
        ctl("plugin", "unload", args.overview_plugin)
        passed("overview open and close")

    ctl("output", "remove", "HEADLESS-2")
    wait_for(lambda: len(json.loads(ctl("-j", "monitors"))) == 1, "monitor was not removed")
    for number, count in [(2, 1), (1, 2)]:
        focus_workspace(number)
        evaluate(f'assert(hl.get_workspace("{number}").tiled_layout == "tabbed")')
        assert len(selected(number)) == 1
        assert len([w for w in windows() if w["workspace"]["id"] == number]) == count
    passed("monitor removal keeps windows reachable and modes intact")

    evaluate('for _,w in ipairs(hl.get_windows()) do assert(hl.dispatch(hl.dsp.window.move({window="address:"..w.address,workspace="1",follow=false})).ok) end')
    ctl("plugin", "unload", str(Path(args.plugin).resolve()))
    assert all(w["visible"] and w["acceptsInput"] for w in windows())
    evaluate('assert(focus_test_ctx.workspace_layouts["1"] == nil); assert(hl.get_workspace("1").tiled_layout == "dwindle")')
    passed("plugin unload restores visibility, input, and honest policy")

    ctl("plugin", "load", str(Path(args.plugin).resolve()))
    evaluate('assert(focus_test_ctx.apply_workspace_layout(hl.get_workspace("1"), "tabbed"))')
    for process, _ in clients:
        if process.poll() is not None:
            continue
        count = len(windows())
        process.terminate()
        wait_for(lambda: len(windows()) == count - 1, "client did not close")
        remaining = windows()
        assert len(selected()) == int(bool(remaining))
        if len(remaining) == 1:
            assert remaining[0]["at"][1] == 21
        evaluate('assert(hl.get_workspace("1").tiled_layout == "tabbed")')
    launch("after-empty")
    assert selected() == ["focus-test-after-empty"] and windows()[0]["at"][1] == 21
    passed("close to one/zero and reopen without singleton tab strip")
finally:
    for process, client_log in clients:
        if process.poll() is None:
            process.terminate()
        process.wait(timeout=10)
        client_log.close()
    if compositor.poll() is None:
        compositor.terminate()
    compositor.wait(timeout=10)
    log.close()
