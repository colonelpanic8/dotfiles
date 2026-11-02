import QtQuick
import Quickshell
import Quickshell.Io
import qs.Common
import qs.Widgets
import qs.DCommon.Widgets
import qs.Modules.Plugins
import qs.Modules.DDash

// Today's org agenda (overdue included) through mova_agenda, which talks to
// org-agenda-api. Captures go through mova_capture.sh's retrying queue.
DashCardComponent {
    id: root

    readonly property bool showHabits: options.habits === true
    readonly property int staleMs: 60 * 1000

    property var entries: []
    property string error: ""
    property bool loading: false
    property double loadedAt: 0
    property bool captureToInbox: false
    property var pendingCompletions: []

    opensTab: false
    pad: Theme.spacingS

    onLiveChanged: {
        if (live && Date.now() - loadedAt > staleMs)
            refresh();
    }
    onShowHabitsChanged: refresh()
    Component.onCompleted: refresh()

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
                "error": text.trim() || "mova_agenda produced no output"
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

    function capture(text) {
        const args = ["mova_capture.sh", "--text", text];
        if (captureToInbox)
            args.push("--inbox");
        Quickshell.execDetached(args);
        // The drain posts right away; pick the new entry up once it lands.
        captureRefresh.restart();
    }

    function subtitle(entry) {
        const parts = [];
        if (entry.overdue) {
            const date = entry.deadline && entry.deadline < (entry.scheduled ?? "9999") ? entry.deadline : entry.scheduled;
            parts.push(I18n.trFor("orgAgenda", "Overdue since %1").arg(Qt.formatDate(new Date(date + "T00:00:00"), "MMM d")));
        } else if (entry.time) {
            const t = new Date("1970-01-01T" + entry.time + ":00");
            parts.push(Qt.formatTime(t, SettingsData.use24HourClock ? "HH:mm" : "h:mm AP"));
        }
        if (entry.dueToday)
            parts.push(I18n.trFor("orgAgenda", "Due today"));
        if (entry.habit)
            parts.push(I18n.trFor("orgAgenda", "Habit"));
        parts.push(entry.todo);
        if (entry.category)
            parts.push(entry.category);
        return parts.join(" · ");
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

    Timer {
        interval: 5 * 60 * 1000
        repeat: true
        running: root.live
        onTriggered: root.refresh()
    }

    Timer {
        id: captureRefresh
        interval: 4000
        onTriggered: root.refresh()
    }

    Item {
        id: header
        anchors.left: parent.left
        anchors.right: parent.right
        anchors.top: parent.top
        height: Theme.buttonHeightXS

        StyledText {
            anchors.left: parent.left
            anchors.leftMargin: Theme.spacingXS
            anchors.right: refreshButton.left
            anchors.verticalCenter: parent.verticalCenter
            text: root.entries.length > 0 ? I18n.trFor("orgAgenda", "Agenda · %1").arg(root.entries.length) : I18n.trFor("orgAgenda", "Agenda")
            font.pixelSize: Theme.fontSizeSmall
            font.weight: Theme.fontWeightMedium
            color: root.mutedColor
            elide: Text.ElideRight
        }

        DActionButton {
            id: refreshButton
            anchors.right: parent.right
            anchors.verticalCenter: parent.verticalCenter
            buttonSize: DashMetrics.eventActionSize
            iconSize: DashMetrics.eventActionIconSize
            iconName: root.loading ? "hourglass_top" : "refresh"
            iconColor: root.mutedColor
            tooltipText: I18n.trFor("orgAgenda", "Refresh")
            onClicked: root.refresh()
        }
    }

    StyledText {
        id: errorText
        anchors.left: parent.left
        anchors.right: parent.right
        anchors.top: captureInput.bottom
        anchors.margins: Theme.spacingXS
        visible: root.error !== ""
        text: root.error
        font.pixelSize: Theme.fontSizeSmall
        color: Theme.error
        wrapMode: Text.Wrap
        maximumLineCount: 2
        elide: Text.ElideRight
    }

    ListView {
        id: list
        anchors.left: parent.left
        anchors.right: parent.right
        anchors.top: errorText.visible ? errorText.bottom : captureInput.bottom
        anchors.bottom: parent.bottom
        anchors.topMargin: Theme.spacingS
        clip: true
        spacing: Theme.spacingXXS
        model: root.entries
        boundsBehavior: Flickable.StopAtBounds

        delegate: Item {
            id: row
            required property var modelData
            width: ListView.view.width
            height: DashMetrics.eventRowMinHeight

            DActionButton {
                id: check
                anchors.left: parent.left
                anchors.verticalCenter: parent.verticalCenter
                buttonSize: DashMetrics.eventActionSize
                iconSize: DashMetrics.eventActionIconSize
                iconName: "check_box_outline_blank"
                iconColor: row.modelData.overdue ? Theme.error : root.mutedColor
                tooltipText: I18n.trFor("orgAgenda", "Complete")
                onClicked: root.complete(row.modelData)
            }

            Column {
                anchors.left: check.right
                anchors.leftMargin: Theme.spacingS
                anchors.right: parent.right
                anchors.verticalCenter: parent.verticalCenter
                spacing: Theme.spacingXXS

                StyledText {
                    width: parent.width
                    text: (row.modelData.priority === "A" ? "[#A] " : "") + row.modelData.title
                    font.pixelSize: Theme.fontSizeMedium
                    font.weight: Theme.fontWeightMedium
                    color: root.contentColor
                    elide: Text.ElideRight
                    maximumLineCount: 1
                }

                StyledText {
                    width: parent.width
                    text: root.subtitle(row.modelData)
                    font.pixelSize: Theme.fontSizeSmall
                    color: row.modelData.overdue ? Theme.error : root.mutedColor
                    elide: Text.ElideRight
                    maximumLineCount: 1
                }
            }
        }

        StyledText {
            anchors.centerIn: parent
            visible: list.count === 0 && !root.loading && root.error === ""
            text: I18n.trFor("orgAgenda", "Nothing on the agenda")
            font.pixelSize: Theme.fontSizeSmall
            color: root.mutedColor
        }
    }

    DTextField {
        id: captureInput
        anchors.left: parent.left
        anchors.right: destinationButton.left
        anchors.rightMargin: Theme.spacingXS
        anchors.top: header.bottom
        anchors.topMargin: Theme.spacingXS
        height: DashMetrics.taskInputHeight
        cornerRadius: Theme.fullRadius(width, height)
        font.pixelSize: Theme.fontSizeSmall
        leftIconName: "add_task"
        leftIconSize: Theme.iconSizeSmall
        hidePlaceholderOnFocus: false
        placeholderText: root.captureToInbox ? I18n.trFor("orgAgenda", "Capture to Inbox…") : I18n.trFor("orgAgenda", "Capture for today…")

        onAccepted: {
            const txt = text.trim();
            if (txt === "")
                return;
            root.capture(txt);
            text = "";
        }
    }

    DActionButton {
        id: destinationButton
        anchors.right: parent.right
        anchors.verticalCenter: captureInput.verticalCenter
        buttonSize: DashMetrics.eventActionSize
        iconSize: DashMetrics.eventActionIconSize
        iconName: root.captureToInbox ? "inbox" : "today"
        iconColor: root.accentColor
        tooltipText: root.captureToInbox ? I18n.trFor("orgAgenda", "Capturing to Inbox (click for Today)") : I18n.trFor("orgAgenda", "Capturing for today (click for Inbox)")
        onClicked: root.captureToInbox = !root.captureToInbox
    }
}
