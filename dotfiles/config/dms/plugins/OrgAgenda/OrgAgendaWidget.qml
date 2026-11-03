import QtQuick
import qs.Common
import qs.Widgets
import qs.DCommon.Widgets
import qs.Modules.Plugins

PluginComponent {
    id: root

    layerNamespacePlugin: "org-agenda"
    popoutWidth: 440
    popoutHeight: 480

    AgendaData {
        id: agendaData
    }

    horizontalBarPill: Component {
        Row {
            spacing: Theme.spacingXS
            height: root.widgetThickness

            DIcon {
                anchors.verticalCenter: parent.verticalCenter
                name: "checklist"
                size: root.iconSize
                color: Theme.widgetIconColor
            }

            StyledText {
                anchors.verticalCenter: parent.verticalCenter
                visible: agendaData.entries.length > 0
                text: agendaData.entries.length
                font.pixelSize: root.textSize
                color: Theme.widgetTextColor
            }
        }
    }

    verticalBarPill: Component {
        Column {
            spacing: Theme.spacingXXS

            DIcon {
                anchors.horizontalCenter: parent.horizontalCenter
                name: "checklist"
                size: root.iconSize
                color: Theme.widgetIconColor
            }

            StyledText {
                anchors.horizontalCenter: parent.horizontalCenter
                visible: agendaData.entries.length > 0
                text: agendaData.entries.length
                font.pixelSize: root.textSize
                color: Theme.widgetTextColor
            }
        }
    }

    popoutContent: Component {
        Item {
            id: popoutRoot
            property var parentPopout: null
            readonly property bool popoutVisible: parentPopout ? parentPopout.shouldBeVisible : false
            implicitHeight: root.popoutHeight

            onPopoutVisibleChanged: {
                if (!popoutVisible)
                    return;
                if (Date.now() - agendaData.loadedAt > agendaData.staleMs)
                    agendaData.refresh();
                Qt.callLater(view.focusCapture);
            }

            AgendaView {
                id: view
                anchors.fill: parent
                agenda: agendaData
                pluginService: root.pluginService
                pluginId: root.pluginId
            }
        }
    }
}
