import QtQuick
import qs.Common
import qs.Services
import qs.Widgets
import qs.DCommon.Widgets
import qs.Modules.Plugins

PluginComponent {
    id: root

    readonly property bool available: DgopService.totalSwapKB > 0
    readonly property real usage: available ? DgopService.usedSwapKB * 100 / DgopService.totalSwapKB : 0

    onAvailableChanged: setVisibilityOverride(available)
    Component.onCompleted: {
        DgopService.addRef(["memory"]);
        setVisibilityOverride(available);
    }
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
                name: "swap_horiz"
                size: root.iconSize
                color: root.usage > 80 ? Theme.tempDanger : root.usage > 50 ? Theme.tempWarning : Theme.widgetIconColor
            }

            StyledText {
                anchors.verticalCenter: parent.verticalCenter
                text: root.usage.toFixed(0) + "%"
                font.pixelSize: root.textSize
                color: Theme.widgetTextColor
            }
        }
    }
}
