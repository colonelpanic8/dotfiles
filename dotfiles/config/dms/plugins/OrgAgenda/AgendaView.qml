import QtQuick
import qs.Common
import qs.Widgets
import qs.DCommon.Widgets
import qs.Modules.DDash

// Agenda and habit lists plus capture through any org-agenda-api capture
// template.
Item {
    id: root

    required property AgendaData agenda
    property var pluginService: null
    property string pluginId: "orgAgenda"
    property color contentColor: Theme.surfaceText
    property color mutedColor: Theme.surfaceVariantText
    property color accentColor: Theme.primary

    readonly property string defaultTemplate: "capture-n"
    property string templateKey: pluginService?.loadPluginState(pluginId, "template", defaultTemplate) ?? defaultTemplate
    readonly property var currentTemplate: agenda.template(templateKey) ?? agenda.template(defaultTemplate) ?? agenda.templates[0]
    readonly property var prompts: currentTemplate?.prompts?.length ? currentTemplate.prompts : [
        {
            "name": "Title",
            "type": "string",
            "required": true
        }
    ]
    property string tab: pluginService?.loadPluginState(pluginId, "tab", "agenda") ?? "agenda"
    readonly property bool showingHabits: tab === "habits"
    readonly property color habitGood: agenda.habitColors.conforming
    readonly property color habitBad: agenda.habitColors.notConforming
    readonly property int habitCell: Theme.spacingS + Theme.spacingXXS
    property bool picking: false
    property string formError: ""

    function focusCapture() {
        captureInput.forceActiveFocus();
    }

    function chooseTab(key) {
        tab = key;
        picking = false;
        pluginService?.savePluginState(pluginId, "tab", key);
    }

    function habitColor(ratio) {
        const t = Math.max(0, Math.min(1, ratio));
        return Qt.rgba(habitBad.r + (habitGood.r - habitBad.r) * t, habitBad.g + (habitGood.g - habitBad.g) * t, habitBad.b + (habitGood.b - habitBad.b) * t, 1);
    }

    function habitSubtitle(habit) {
        const parts = (habit.windows ?? []).map(w => I18n.trFor("orgAgenda", "%1/%2 in %3d").arg(w.done).arg(w.target).arg(w.days));
        if (habit.done)
            parts.unshift(I18n.trFor("orgAgenda", "Done today"));
        else if (!habit.needed)
            parts.unshift(I18n.trFor("orgAgenda", "Optional today"));
        return parts.join(" · ");
    }

    function chooseTemplate(key) {
        templateKey = key;
        picking = false;
        formError = "";
        pluginService?.savePluginState(pluginId, "template", key);
        focusCapture();
    }

    function fieldAt(i) {
        return i === 0 ? captureInput : extraFields.itemAt(i - 1);
    }

    function hint(prompt) {
        const name = prompt.required ? prompt.name : I18n.trFor("orgAgenda", "%1 (optional)").arg(prompt.name);
        if (prompt.type === "date")
            return I18n.trFor("orgAgenda", "%1: today, fri, +3, 2026-11-05 14:00").arg(name);
        if (prompt.type === "tags")
            return I18n.trFor("orgAgenda", "%1, comma separated").arg(name);
        return name + "…";
    }

    // Accepts today/tomorrow, weekday names, +N days or an ISO date, with an
    // optional HH:MM. Returns the API's date[THH:MM] form, or "" if unparseable.
    function parseWhen(input) {
        const m = input.trim().toLowerCase().replace(/^(\d{4}-\d{2}-\d{2})t/, "$1 ").match(/^(\S+)(?:\s+(\d{1,2}):(\d{2}))?$/);
        if (!m)
            return "";
        const day = new Date();
        day.setHours(0, 0, 0, 0);
        const token = m[1];
        const weekdays = ["sun", "mon", "tue", "wed", "thu", "fri", "sat"];
        const weekday = weekdays.indexOf(token.slice(0, 3));
        let rel;
        if (token === "today" || token === "tod")
            rel = 0;
        else if (["tomorrow", "tom", "tmr"].includes(token))
            rel = 1;
        else if ((rel = token.match(/^\+(\d+)d?$/)))
            rel = parseInt(rel[1]);
        else if (weekday >= 0 && weekdays[weekday] === token.slice(0, 3) && /^[a-z]+$/.test(token))
            rel = (weekday - day.getDay() + 7) % 7 || 7;
        else if (/^\d{4}-\d{2}-\d{2}$/.test(token))
            rel = null;
        else
            return "";
        let date = token;
        if (rel !== null) {
            day.setDate(day.getDate() + rel);
            date = Qt.formatDate(day, "yyyy-MM-dd");
        }
        if (!m[2])
            return date;
        const h = parseInt(m[2]);
        const min = parseInt(m[3]);
        if (h > 23 || min > 59)
            return "";
        return date + "T" + (h < 10 ? "0" : "") + h + ":" + m[3];
    }

    function submit() {
        const values = {};
        for (let i = 0; i < prompts.length; i++) {
            const prompt = prompts[i];
            const field = fieldAt(i);
            const text = field ? field.text.trim() : "";
            if (text === "") {
                if (prompt.required) {
                    field?.forceActiveFocus();
                    return;
                }
                continue;
            }
            let value = text;
            if (prompt.type === "date") {
                value = parseWhen(text);
                if (value === "") {
                    formError = I18n.trFor("orgAgenda", "Couldn't read “%1” as a date").arg(text);
                    field.forceActiveFocus();
                    return;
                }
            }
            values[prompt.name] = value;
        }
        agenda.capture(currentTemplate.key, values);
        formError = "";
        for (let i = 0; i < prompts.length; i++)
            fieldAt(i)?.clear();
        focusCapture();
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

    Keys.onEscapePressed: event => {
        if (picking) {
            picking = false;
            event.accepted = true;
        } else {
            event.accepted = false;
        }
    }

    Item {
        id: header
        anchors.left: parent.left
        anchors.right: parent.right
        anchors.top: parent.top
        height: Theme.buttonHeightXS

        Row {
            anchors.left: parent.left
            anchors.verticalCenter: parent.verticalCenter
            height: parent.height
            spacing: Theme.spacingXXS

            Repeater {
                model: [
                    {
                        "key": "agenda",
                        "label": I18n.trFor("orgAgenda", "Agenda"),
                        "count": root.agenda.entries.length
                    },
                    {
                        "key": "habits",
                        "label": I18n.trFor("orgAgenda", "Habits"),
                        "count": root.agenda.habitsDue
                    }
                ]

                StyledRect {
                    id: tabButton
                    required property var modelData
                    readonly property bool selected: root.tab === modelData.key
                    height: parent.height
                    width: tabLabel.implicitWidth + Theme.spacingS * 2
                    radius: Theme.cornerRadius
                    color: selected ? Theme.chipSurface : tabMouse.containsMouse ? Theme.withAlpha(Theme.chipSurface, 0.5) : "transparent"

                    StyledText {
                        id: tabLabel
                        anchors.centerIn: parent
                        text: tabButton.modelData.count > 0 ? "%1 · %2".arg(tabButton.modelData.label).arg(tabButton.modelData.count) : tabButton.modelData.label
                        font.pixelSize: Theme.fontSizeSmall
                        font.weight: Theme.fontWeightMedium
                        color: tabButton.selected ? root.contentColor : root.mutedColor
                    }

                    MouseArea {
                        id: tabMouse
                        anchors.fill: parent
                        hoverEnabled: true
                        cursorShape: Qt.PointingHandCursor
                        onClicked: root.chooseTab(tabButton.modelData.key)
                    }
                }
            }
        }

        DActionButton {
            id: refreshButton
            anchors.right: parent.right
            anchors.verticalCenter: parent.verticalCenter
            buttonSize: DashMetrics.eventActionSize
            iconSize: DashMetrics.eventActionIconSize
            iconName: root.agenda.loading || root.agenda.habitsLoading ? "hourglass_top" : "refresh"
            iconColor: root.mutedColor
            tooltipText: I18n.trFor("orgAgenda", "Refresh")
            onClicked: root.agenda.refresh()
        }
    }

    Column {
        id: captureBox
        anchors.left: parent.left
        anchors.right: parent.right
        anchors.top: header.bottom
        anchors.topMargin: Theme.spacingXS
        spacing: Theme.spacingXS

        Item {
            width: parent.width
            height: DashMetrics.taskInputHeight

            TextMetrics {
                id: chipMetrics
                font.pixelSize: Theme.fontSizeSmall
                text: root.currentTemplate?.name ?? ""
            }

            StyledRect {
                id: templateChip
                anchors.left: parent.left
                anchors.verticalCenter: parent.verticalCenter
                height: parent.height
                width: Math.min(Math.ceil(chipMetrics.advanceWidth) + chipIcon.size + Theme.spacingXXS + Theme.spacingM * 2, parent.width * 0.4)
                radius: captureInput.cornerRadius
                color: root.picking || chipMouse.containsMouse ? Qt.tint(Theme.chipSurface, Theme.withAlpha(root.accentColor, 0.12)) : Theme.chipSurface
                border.width: 1
                border.color: root.picking ? root.accentColor : Theme.outlineVariant

                StyledText {
                    anchors.left: parent.left
                    anchors.leftMargin: Theme.spacingM
                    anchors.right: chipIcon.left
                    anchors.rightMargin: Theme.spacingXXS
                    anchors.verticalCenter: parent.verticalCenter
                    text: chipMetrics.text
                    font.pixelSize: Theme.fontSizeSmall
                    color: root.accentColor
                    elide: Text.ElideRight
                    wrapMode: Text.NoWrap
                    maximumLineCount: 1
                }

                DIcon {
                    id: chipIcon
                    anchors.right: parent.right
                    anchors.rightMargin: Theme.spacingM
                    anchors.verticalCenter: parent.verticalCenter
                    name: root.picking ? "expand_less" : "expand_more"
                    size: Theme.iconSizeSmall
                    color: root.accentColor
                }

                MouseArea {
                    id: chipMouse
                    anchors.fill: parent
                    hoverEnabled: true
                    cursorShape: Qt.PointingHandCursor
                    onClicked: root.picking = !root.picking
                }
            }

            DTextField {
                id: captureInput
                anchors.left: templateChip.right
                anchors.leftMargin: Theme.spacingXS
                anchors.right: parent.right
                anchors.verticalCenter: parent.verticalCenter
                height: parent.height
                cornerRadius: Theme.fullRadius(width, height)
                font.pixelSize: Theme.fontSizeSmall
                leftIconName: "add_task"
                leftIconSize: Theme.iconSizeSmall
                hidePlaceholderOnFocus: false
                placeholderText: root.hint(root.prompts[0])
                onAccepted: root.submit()
            }
        }

        Repeater {
            id: extraFields
            model: root.prompts.slice(1)

            DTextField {
                required property var modelData
                width: captureBox.width
                height: DashMetrics.taskInputHeight
                cornerRadius: Theme.fullRadius(width, height)
                font.pixelSize: Theme.fontSizeSmall
                leftIconName: modelData.type === "date" ? "event" : modelData.type === "tags" ? "sell" : "notes"
                leftIconSize: Theme.iconSizeSmall
                hidePlaceholderOnFocus: false
                placeholderText: root.hint(modelData)
                onAccepted: root.submit()
            }
        }
    }

    StyledText {
        id: errorText
        anchors.left: parent.left
        anchors.right: parent.right
        anchors.top: captureBox.bottom
        anchors.margins: Theme.spacingXS
        readonly property string message: root.formError || (root.showingHabits ? root.agenda.habitsError : root.agenda.error)
        visible: message !== ""
        text: message
        font.pixelSize: Theme.fontSizeSmall
        color: Theme.error
        wrapMode: Text.Wrap
        maximumLineCount: 2
        elide: Text.ElideRight
    }

    ListView {
        id: templateList
        anchors.left: parent.left
        anchors.right: parent.right
        anchors.top: errorText.visible ? errorText.bottom : captureBox.bottom
        anchors.bottom: parent.bottom
        anchors.topMargin: Theme.spacingS
        visible: root.picking
        clip: true
        spacing: Theme.spacingXXS
        model: root.agenda.templates
        boundsBehavior: Flickable.StopAtBounds

        delegate: StyledRect {
            id: templateRow
            required property var modelData
            readonly property bool current: modelData.key === root.currentTemplate?.key
            width: ListView.view.width
            height: Theme.buttonHeightXS + Theme.spacingXS
            radius: Theme.cornerRadius
            color: rowMouse.containsMouse ? Theme.chipSurface : "transparent"

            DIcon {
                id: templateIcon
                anchors.left: parent.left
                anchors.leftMargin: Theme.spacingS
                anchors.verticalCenter: parent.verticalCenter
                name: templateRow.current ? "radio_button_checked" : "radio_button_unchecked"
                size: Theme.iconSizeSmall
                color: templateRow.current ? root.accentColor : root.mutedColor
            }

            StyledText {
                anchors.left: templateIcon.right
                anchors.leftMargin: Theme.spacingS
                anchors.right: fieldsText.left
                anchors.rightMargin: Theme.spacingS
                anchors.verticalCenter: parent.verticalCenter
                text: templateRow.modelData.name
                font.pixelSize: Theme.fontSizeMedium
                color: root.contentColor
                elide: Text.ElideRight
            }

            StyledText {
                id: fieldsText
                anchors.right: parent.right
                anchors.rightMargin: Theme.spacingS
                anchors.verticalCenter: parent.verticalCenter
                text: (templateRow.modelData.prompts ?? []).map(p => p.name).join(", ")
                font.pixelSize: Theme.fontSizeSmall
                color: root.mutedColor
            }

            MouseArea {
                id: rowMouse
                anchors.fill: parent
                hoverEnabled: true
                cursorShape: Qt.PointingHandCursor
                onClicked: root.chooseTemplate(templateRow.modelData.key)
            }
        }
    }

    ListView {
        id: list
        anchors.fill: templateList
        visible: !root.picking && !root.showingHabits
        clip: true
        spacing: Theme.spacingXXS
        model: root.agenda.entries
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
                onClicked: root.agenda.complete(row.modelData)
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
            visible: list.count === 0 && !root.agenda.loading && root.agenda.error === ""
            text: I18n.trFor("orgAgenda", "Nothing on the agenda")
            font.pixelSize: Theme.fontSizeSmall
            color: root.mutedColor
        }
    }

    ListView {
        id: habitList
        anchors.fill: templateList
        visible: !root.picking && root.showingHabits
        clip: true
        spacing: Theme.spacingXXS
        model: root.agenda.habits
        boundsBehavior: Flickable.StopAtBounds

        delegate: Item {
            id: habitRow
            required property var modelData
            width: ListView.view.width
            height: DashMetrics.eventRowMinHeight

            DActionButton {
                id: habitCheck
                anchors.left: parent.left
                anchors.verticalCenter: parent.verticalCenter
                buttonSize: DashMetrics.eventActionSize
                iconSize: DashMetrics.eventActionIconSize
                iconName: habitRow.modelData.done ? "check_box" : "check_box_outline_blank"
                iconColor: habitRow.modelData.done ? root.mutedColor : habitRow.modelData.needed ? root.accentColor : root.mutedColor
                enabled: !habitRow.modelData.done
                tooltipText: I18n.trFor("orgAgenda", "Complete")
                onClicked: root.agenda.completeHabit(habitRow.modelData)
            }

            Column {
                anchors.left: habitCheck.right
                anchors.leftMargin: Theme.spacingS
                anchors.right: graph.left
                anchors.rightMargin: Theme.spacingS
                anchors.verticalCenter: parent.verticalCenter
                spacing: Theme.spacingXXS

                StyledText {
                    width: parent.width
                    text: habitRow.modelData.title
                    font.pixelSize: Theme.fontSizeMedium
                    font.weight: Theme.fontWeightMedium
                    color: habitRow.modelData.done ? root.mutedColor : root.contentColor
                    elide: Text.ElideRight
                    maximumLineCount: 1
                }

                StyledText {
                    width: parent.width
                    text: root.habitSubtitle(habitRow.modelData)
                    font.pixelSize: Theme.fontSizeSmall
                    color: root.mutedColor
                    elide: Text.ElideRight
                    maximumLineCount: 1
                }
            }

            // Right-aligned so today's cell lines up across habits of any age.
            Row {
                id: graph
                anchors.right: parent.right
                anchors.rightMargin: Theme.spacingXS
                anchors.verticalCenter: parent.verticalCenter
                spacing: Theme.spacingXXS

                Repeater {
                    model: habitRow.modelData.graph

                    Rectangle {
                        id: cell
                        required property var modelData
                        readonly property bool future: modelData.status === "future"
                        readonly property bool present: modelData.status === "present"
                        width: root.habitCell
                        height: root.habitCell * (present ? 1.5 : 1)
                        anchors.verticalCenter: parent.verticalCenter
                        radius: Theme.spacingXXS
                        color: future ? "transparent" : root.habitColor(modelData.ratio)
                        border.width: future ? 1 : 0
                        border.color: Theme.outlineVariant

                        // What completing today would bring the habit to.
                        Rectangle {
                            visible: cell.present && cell.modelData.count === 0 && cell.modelData.ratioWith !== cell.modelData.ratio
                            anchors.left: parent.left
                            anchors.right: parent.right
                            anchors.bottom: parent.bottom
                            height: parent.height / 2
                            bottomLeftRadius: parent.radius
                            bottomRightRadius: parent.radius
                            color: root.habitColor(cell.modelData.ratioWith)
                        }

                        Rectangle {
                            visible: cell.modelData.count > 0
                            anchors.centerIn: parent
                            width: Math.round(root.habitCell / 3)
                            height: width
                            radius: width / 2
                            color: "#FFFFFF"
                        }
                    }
                }
            }
        }

        StyledText {
            anchors.centerIn: parent
            visible: habitList.count === 0 && !root.agenda.habitsLoading && root.agenda.habitsError === ""
            text: I18n.trFor("orgAgenda", "No habits")
            font.pixelSize: Theme.fontSizeSmall
            color: root.mutedColor
        }
    }
}
