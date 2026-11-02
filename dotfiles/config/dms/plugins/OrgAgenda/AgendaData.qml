import QtQuick
import Quickshell
import Quickshell.Io

// Today's org agenda (overdue included) through mova_agenda, which talks to
// org-agenda-api. Captures go through mova_capture.sh's retrying queue.
Item {
    id: root

    property bool showHabits: false
    // Refresh periodically while true, and on becoming true if stale.
    property bool active: true

    readonly property int staleMs: 60 * 1000
    readonly property var fallbackTemplates: [
        {
            "key": "capture-n",
            "name": "Next (Scheduled Today)",
            "prompts": []
        },
        {
            "key": "capture-i",
            "name": "Inbox",
            "prompts": []
        }
    ]

    property var entries: []
    property string error: ""
    property bool loading: false
    property double loadedAt: 0
    property var templates: fallbackTemplates
    property var pendingCompletions: []

    readonly property int overdueCount: entries.filter(e => e.overdue).length

    onActiveChanged: {
        if (active && Date.now() - loadedAt > staleMs)
            refresh();
    }
    onShowHabitsChanged: refresh()
    Component.onCompleted: {
        refresh();
        templatesProc.running = true;
    }

    function refresh() {
        if (agendaProc.running) {
            agendaProc.again = true;
            return;
        }
        loading = true;
        agendaProc.command = showHabits ? ["mova_agenda", "today", "--habits"] : ["mova_agenda", "today"];
        agendaProc.running = true;
    }

    function parse(text) {
        try {
            return JSON.parse(text);
        } catch (e) {
            return {
                "error": text.trim() || "no output"
            };
        }
    }

    function complete(entry) {
        entries = entries.filter(e => e !== entry);
        pendingCompletions = pendingCompletions.concat([JSON.stringify(entry)]);
        runNextCompletion();
    }

    function runNextCompletion() {
        if (completeProc.running || pendingCompletions.length === 0)
            return;
        completeProc.command = ["mova_agenda", "complete", pendingCompletions[0]];
        pendingCompletions = pendingCompletions.slice(1);
        completeProc.running = true;
    }

    function template(key) {
        return templates.find(t => t.key === key) ?? null;
    }

    function capture(key, values) {
        Quickshell.execDetached(["mova_capture.sh", "--values", key, JSON.stringify(values)]);
        // The drain posts right away; pick the new entry up once it lands.
        captureRefresh.restart();
    }

    Process {
        id: agendaProc
        property bool again: false
        stdout: StdioCollector {
            onStreamFinished: {
                const result = root.parse(text);
                if (Array.isArray(result)) {
                    root.entries = result;
                    root.error = "";
                    root.loadedAt = Date.now();
                } else {
                    root.error = result.error ?? "Unexpected response";
                }
            }
        }
        onExited: {
            root.loading = false;
            if (again) {
                again = false;
                root.refresh();
            }
        }
    }

    Process {
        id: completeProc
        stdout: StdioCollector {
            onStreamFinished: {
                const result = root.parse(text);
                if (result.error)
                    root.error = result.error;
            }
        }
        onExited: {
            if (root.pendingCompletions.length > 0)
                root.runNextCompletion();
            else
                root.refresh();
        }
    }

    Process {
        id: templatesProc
        command: ["mova_capture.sh", "--templates"]
        stdout: StdioCollector {
            onStreamFinished: {
                const result = root.parse(text);
                if (!result || typeof result !== "object" || Array.isArray(result) || result.error)
                    return;
                const list = Object.keys(result).map(key => ({
                            "key": key,
                            "name": result[key].name || key,
                            "prompts": result[key].prompts || []
                        }));
                if (list.length > 0)
                    root.templates = list;
            }
        }
    }

    Timer {
        interval: 5 * 60 * 1000
        repeat: true
        running: root.active
        onTriggered: root.refresh()
    }

    Timer {
        // Templates rarely change; pick up edits without a shell restart.
        interval: 60 * 60 * 1000
        repeat: true
        running: true
        onTriggered: templatesProc.running = true
    }

    Timer {
        id: captureRefresh
        interval: 4000
        onTriggered: root.refresh()
    }
}
