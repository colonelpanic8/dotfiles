import QtQuick
import qs.Common
import qs.Services
import qs.Widgets
import qs.DCommon.Widgets
import qs.Modules.Plugins

PluginComponent {
    id: root

    // Same device selection as the control center's brightness group.
    readonly property string pinKey: parentScreen ? SettingsData.getScreenDisplayName(parentScreen) : ""
    readonly property var targets: {
        const pinned = BrightnessService.pinnedDevices(pinKey);
        return pinned.length > 0 ? pinned : [BrightnessService.getDefaultDevice()];
    }
    readonly property string device: targets[0] || ""
    readonly property real level: device ? BrightnessService.getDeviceBrightness(device) : NaN
    readonly property bool available: BrightnessService.brightnessAvailable && device !== ""
    property real accumulator: 0

    onAvailableChanged: setVisibilityOverride(available)
    Component.onCompleted: setVisibilityOverride(available)

    pillClickAction: (x, y, width, section, screen) => PopoutService.toggleControlCenter(x, y, width, section, screen)

    function adjust(delta) {
        let step = 5;
        if (Math.abs(delta) < 120 || Math.abs(delta) % 120 !== 0) {
            step = 1;
            accumulator += delta;
            if (Math.abs(accumulator) < 100)
                return;
            delta = accumulator;
            accumulator = 0;
        }
        const current = BrightnessService.getDeviceBrightness(device);
        BrightnessService.setGroupBrightness(delta > 0 ? Math.min(100, current + step) : Math.max(1, current - step), targets);
    }

    horizontalBarPill: Component {
        Item {
            implicitWidth: row.implicitWidth
            implicitHeight: root.widgetThickness

            Row {
                id: row
                anchors.verticalCenter: parent.verticalCenter
                spacing: Theme.spacingXS

                DIcon {
                    anchors.verticalCenter: parent.verticalCenter
                    name: BrightnessService.brightnessIconName(BrightnessService.getCurrentDeviceInfoByName(root.device), root.level)
                    size: root.iconSize
                    color: Theme.widgetIconColor
                }

                StyledText {
                    anchors.verticalCenter: parent.verticalCenter
                    text: isFinite(root.level) ? root.level.toFixed(0) + "%" : "--%"
                    font.pixelSize: root.textSize
                    color: Theme.widgetTextColor
                }
            }

            // The pill passes wheel events through to its content.
            MouseArea {
                anchors.fill: parent
                acceptedButtons: Qt.NoButton
                onWheel: wheel => root.adjust(wheel.angleDelta.y)
            }
        }
    }
}
