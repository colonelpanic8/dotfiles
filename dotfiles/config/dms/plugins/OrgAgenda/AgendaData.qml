import QtQuick
import Quickshell
import Quickshell.Io

// Today's org agenda (overdue included) and window habits through
// mova_agenda, which talks to org-agenda-api. Captures go through
// mova_capture.sh's retrying queue.
Item {
    id: root

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
    property var habits: []
    property var habitColors: ({
            "conforming": "#00B0FF",
            "notConforming": "#F44336"
        })
    property string habitsError: ""
    property bool habitsLoading: false

    readonly property int overdueCount: entries.filter(e => e.overdue).length
    readonly property int habitsDue: habits.filter(h => h.needed && !h.done).length

    onActiveChanged: {
        if (active && Date.now() - loadedAt > staleMs)
            refresh();
    }
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
        agendaProc.running = true;
        refreshHabits();
    }

    function refreshHabits() {
        if (habitsProc.running) {
            habitsProc.again = true;
            return;
        }
        habitsLoading = true;
        habitsProc.running = true;
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

    function completeHabit(habit) {
        habits = habits.map(h => h.id !== habit.id ? h : Object.assign({}, h, {
                "done": true,
                "graph": h.graph.map(g => g.status !== "present" ? g : Object.assign({}, g, {
                            "count": g.count + 1,
                            "ratio": g.ratioWith
                        }))
            }));
        pendingCompletions = pendingCompletions.concat([JSON.stringify({
                "id": habit.id,
                "title": habit.title
            })]);
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
        command: ["mova_agenda", "today"]
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
        id: habitsProc
        property bool again: false
        command: ["mova_agenda", "habits"]
        stdout: StdioCollector {
            onStreamFinished: {
                const result = root.parse(text);
                if (Array.isArray(result?.habits)) {
                    root.habits = result.habits;
                    root.habitColors = result.colors;
                    root.habitsError = "";
                } else {
                    root.habitsError = result?.error ?? "Unexpected response";
                }
            }
        }
        onExited: {
            root.habitsLoading = false;
            if (again) {
                again = false;
                root.refreshHabits();
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
