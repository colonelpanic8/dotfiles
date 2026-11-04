pragma ComponentBehavior: Bound

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
    // LED devices other than keyboard backlights are status lights, not dimmers.
    readonly property var otherDevices: BrightnessService.devices.filter(d => !targets.includes(d.id) && (d.class !== "leds" || d.id.includes("kbd_backlight")))
    property real accumulator: 0

    layerNamespacePlugin: "desktop-brightness"
    popoutWidth: 380

    onAvailableChanged: setVisibilityOverride(available)
    Component.onCompleted: setVisibilityOverride(available)

    pillRightClickAction: (x, y, width, section, screen) => PopoutService.toggleControlCenter(x, y, width, section, screen)

    function deviceLabel(id) {
        const info = BrightnessService.getCurrentDeviceInfoByName(id);
        const target = id.split(":").slice(1).join(":");
        switch (info?.class) {
        case "backlight":
            return BrightnessService.deviceTitle(info);
        case "ddc":
            return "Monitor (DDC " + target + ")";
        case "software":
            return target + " (software dimming)";
        case "leds":
            return "Keyboard backlight";
        default:
            return id;
        }
    }

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

    component BrightnessRow: Column {
        id: row

        required property var ids
        required property string title

        readonly property var info: BrightnessService.getCurrentDeviceInfoByName(ids[0])
        readonly property real level: {
            BrightnessService.brightnessVersion;
            return BrightnessService.getDeviceBrightness(ids[0]);
        }

        width: parent.width
        spacing: Theme.spacingXS

        StyledText {
            width: parent.width
            text: row.title
            font.pixelSize: Theme.fontSizeSmall
            font.weight: Theme.fontWeightMedium
            color: Theme.primary
            elide: Text.ElideRight
        }

        DSlider {
            id: slider
            width: parent.width
            size: "m"
            minimum: BrightnessService.brightnessMinimum(row.info)
            maximum: BrightnessService.brightnessMaximum(row.info)
            unit: BrightnessService.brightnessUnit(row.info)
            startIcon: BrightnessService.brightnessIconName(row.info, row.level)
            showValue: true
            Accessible.name: row.title
            onSliderValueChanged: newValue => BrightnessService.setGroupBrightness(newValue, row.ids, true)

            Binding on value {
                value: row.level
                when: !slider.isDragging
                restoreMode: Binding.RestoreNone
            }
        }
    }

    popoutContent: Component {
        PopoutComponent {
            headerText: "Brightness"
            detailsText: root.available ? "" : "No controllable display"

            Column {
                width: parent.width
                spacing: Theme.spacingM

                BrightnessRow {
                    visible: root.available
                    ids: root.targets
                    title: root.targets.length > 1 ? root.pinKey : root.deviceLabel(root.device)
                }

                Repeater {
                    model: root.otherDevices

                    BrightnessRow {
                        required property var modelData
                        ids: [modelData.id]
                        title: root.deviceLabel(modelData.id)
                    }
                }

                StyledText {
                    width: parent.width
                    text: "Scroll over the bar pill to adjust; right-click opens the control center."
                    font.pixelSize: Theme.fontSizeSmall
                    color: Theme.surfaceVariantText
                    wrapMode: Text.WordWrap
                }
            }
        }
    }
}
