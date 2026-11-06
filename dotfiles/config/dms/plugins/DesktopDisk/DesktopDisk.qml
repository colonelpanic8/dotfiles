import QtQuick
import qs.Common
import qs.Services
import qs.Widgets
import qs.DCommon.Widgets
import qs.Modules.Plugins

PluginComponent {
    id: root

    readonly property var mount: {
        const mounts = DgopService.diskMounts || [];
        return mounts.find(entry => entry.mount === "/") ?? mounts[0] ?? null;
    }
    readonly property real usage: parseFloat((mount?.percent ?? "").replace("%", "")) || 0
    readonly property real rowPitch: Math.max(6, Math.floor((widgetThickness - 2) / 2))
    readonly property real stackTextSize: Math.max(7, Math.min(Theme.fontSizeMedium, Math.round(rowPitch * 0.8)))

    Component.onCompleted: DgopService.addRef(["diskmounts"])
    Component.onDestruction: DgopService.removeRef(["diskmounts"])

    horizontalBarPill: Component {
        Row {
            spacing: Theme.spacingXS
            height: root.widgetThickness

            DIcon {
                anchors.verticalCenter: parent.verticalCenter
                name: "storage"
                size: root.iconSize
                color: root.usage > 90 ? Theme.tempDanger : root.usage > 75 ? Theme.tempWarning : Theme.widgetIconColor
            }

            Column {
                anchors.verticalCenter: parent.verticalCenter

                NumericText {
                    anchors.horizontalCenter: parent.horizontalCenter
                    height: root.rowPitch
                    verticalAlignment: Text.AlignVCenter
                    text: root.usage ? root.usage.toFixed(0) + "%" : "--%"
                    reserveText: "88%"
                    font.pixelSize: root.stackTextSize
                    color: Theme.widgetTextColor
                }

                NumericText {
                    anchors.horizontalCenter: parent.horizontalCenter
                    height: root.rowPitch
                    verticalAlignment: Text.AlignVCenter
                    text: root.mount?.avail || "--"
                    font.pixelSize: root.stackTextSize
                    color: Theme.surfaceVariantText
                }
            }
        }
    }
}
