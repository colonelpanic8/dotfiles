pragma Singleton

import QtQuick
import Quickshell
import Quickshell.Io

Singleton {
    id: root

    property var state: ({})

    function trayInfo(item) {
        return state.tray?.[item?.watcherAddress] || {
            key: "item-id:" + item?.id,
            priority: 0
        };
    }

    function priority(item) {
        return trayInfo(item).priority;
    }

    function adjustPriority(item, delta) {
        Quickshell.execDetached(["dms-dotfiles-state", "--priority", trayInfo(item).key, String(delta)]);
    }

    function activate(item) {
        if (item?.id !== "localsend_app" && !(item?.title || "").endsWith("localsend_app"))
            return false;
        Quickshell.execDetached(["hyprctl", "eval", "_G.im_hyprland_toggle_localsend_scratchpad()"]);
        return true;
    }

    Process {
        id: collector
        command: ["dms-dotfiles-state", "--watch"]
        running: true
        stdout: SplitParser {
            onRead: data => {
                try {
                    root.state = JSON.parse(data);
                } catch (error) {
                    console.warn("Invalid desktop integration state:", error);
                }
            }
        }
        onExited: retry.restart()
    }

    Timer {
        id: retry
        interval: 5000
        onTriggered: collector.running = true
    }
}
