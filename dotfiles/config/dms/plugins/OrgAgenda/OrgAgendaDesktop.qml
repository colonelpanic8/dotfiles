import QtQuick
import qs.Common
import qs.Modules.Plugins

DesktopPluginComponent {
    id: root

    readonly property bool acceptsKeyboardFocus: true
    readonly property real defaultWidth: 420
    readonly property real defaultHeight: 480

    minWidth: 300
    minHeight: 260

    AgendaData {
        id: agendaData
        showHabits: root.pluginData.habits === true
    }

    Rectangle {
        anchors.fill: parent
        radius: Theme.cornerRadius
        color: Theme.withAlpha(Theme.hostSurface, (root.pluginData.backgroundOpacity ?? 85) / 100)
        border.width: 1
        border.color: Theme.outlineVariant
    }

    AgendaView {
        anchors.fill: parent
        anchors.margins: Theme.spacingM
        agenda: agendaData
        pluginService: root.pluginService
        pluginId: root.pluginId
    }
}
