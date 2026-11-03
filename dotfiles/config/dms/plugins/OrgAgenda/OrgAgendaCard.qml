import QtQuick
import qs.Common
import qs.Modules.Plugins

DashCardComponent {
    id: root

    opensTab: false
    pad: Theme.spacingS

    AgendaData {
        id: agendaData
        active: root.live
    }

    AgendaView {
        anchors.fill: parent
        agenda: agendaData
        pluginService: root.pluginService
        pluginId: root.pluginId
        contentColor: root.contentColor
        mutedColor: root.mutedColor
        accentColor: root.accentColor
    }
}
