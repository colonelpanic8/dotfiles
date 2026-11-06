import QtQuick
import qs.Common
import qs.Services
import qs.Widgets
import qs.DCommon.Widgets
import qs.Modules.Plugins

PluginComponent {
    id: root

    readonly property real ramUsage: DgopService.memoryUsage
    readonly property bool hasSwap: DgopService.totalSwapKB > 0
    readonly property real swapUsage: hasSwap ? DgopService.usedSwapKB * 100 / DgopService.totalSwapKB : 0
    readonly property real rowPitch: Math.max(6, Math.floor((widgetThickness - 2) / 2))
    readonly property real stackTextSize: Math.max(7, Math.min(Theme.fontSizeMedium, Math.round(rowPitch * 0.8)))

    function levelColor(value, warn, danger, fallback) {
        return value > danger ? Theme.tempDanger : value > warn ? Theme.tempWarning : fallback;
    }

    Component.onCompleted: DgopService.addRef(["memory"])
    Component.onDestruction: DgopService.removeRef(["memory"])

    pillClickAction: (x, y, width, section, screen) => {
        DgopService.setSortBy("memory");
        PopoutService.toggleProcessList(x, y, width, section, screen);
    }

    horizontalBarPill: Component {
        Row {
            spacing: Theme.spacingXS
            height: root.widgetThickness

            DIcon {
                anchors.verticalCenter: parent.verticalCenter
                name: "developer_board"
                size: root.iconSize
                color: root.levelColor(root.ramUsage, 75, 90, Theme.widgetIconColor)
            }

            Column {
                anchors.verticalCenter: parent.verticalCenter

                NumericText {
                    height: root.hasSwap ? root.rowPitch : implicitHeight
                    verticalAlignment: Text.AlignVCenter
                    text: (root.hasSwap ? "ram " : "") + (root.ramUsage ? root.ramUsage.toFixed(0) : "--") + "%"
                    reserveText: (root.hasSwap ? "ram " : "") + "88%"
                    font.pixelSize: root.hasSwap ? root.stackTextSize : root.textSize
                    color: Theme.widgetTextColor
                }

                NumericText {
                    visible: root.hasSwap
                    height: root.rowPitch
                    verticalAlignment: Text.AlignVCenter
                    text: "swp " + root.swapUsage.toFixed(0) + "%"
                    reserveText: "swp 88%"
                    font.pixelSize: root.stackTextSize
                    color: root.levelColor(root.swapUsage, 50, 80, Theme.surfaceVariantText)
                }
            }
        }
    }
}
