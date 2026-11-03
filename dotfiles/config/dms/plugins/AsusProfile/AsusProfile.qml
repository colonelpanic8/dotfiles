pragma ComponentBehavior: Bound

import QtQuick
import Quickshell
import qs.Common
import qs.Services
import qs.Widgets
import qs.DCommon.Widgets
import qs.Modules.Plugins

PluginComponent {
    id: root

    readonly property string busName: "xyz.ljones.Asusd"
    readonly property string objectPath: "/xyz/ljones"
    readonly property string iface: "xyz.ljones.Platform"
    // asusd's enum values.
    readonly property var profiles: [
        {
            value: 2,
            label: "Quiet",
            icon: "eco"
        },
        {
            value: 0,
            label: "Balanced",
            icon: "speed"
        },
        {
            value: 1,
            label: "Performance",
            icon: "bolt"
        }
    ]

    property bool available: false
    property var choices: []
    property int live: -1
    property int onAc: -1
    property int onBattery: -1
    property string subscriptionId: ""

    readonly property var shownProfiles: profiles.filter(p => choices.length === 0 || choices.includes(p.value))
    readonly property var liveInfo: profileInfo(live)
    readonly property bool dbusReady: DMSService.isConnected && DMSService.capabilities.includes("dbus")

    layerNamespacePlugin: "asus-profile"
    popoutWidth: 460

    onAvailableChanged: setVisibilityOverride(available)
    onDbusReadyChanged: refresh()
    Component.onCompleted: {
        setVisibilityOverride(available);
        refresh();
    }
    Component.onDestruction: {
        if (subscriptionId)
            DMSService.dbusUnsubscribe(subscriptionId);
    }

    function profileInfo(value) {
        return profiles.find(p => p.value === value) ?? null;
    }

    function applyProps(props) {
        if (props.PlatformProfile !== undefined)
            live = props.PlatformProfile;
        if (props.PlatformProfileOnAc !== undefined)
            onAc = props.PlatformProfileOnAc;
        if (props.PlatformProfileOnBattery !== undefined)
            onBattery = props.PlatformProfileOnBattery;
        if (props.PlatformProfileChoices !== undefined)
            choices = props.PlatformProfileChoices;
    }

    function refresh() {
        if (!dbusReady)
            return;
        DMSService.dbusGetAllProperties("system", busName, objectPath, iface, response => {
            available = !response.error && response.result?.PlatformProfile !== undefined;
            if (available)
                applyProps(response.result);
        });
        if (!subscriptionId)
            DMSService.dbusSubscribe("system", busName, objectPath, "org.freedesktop.DBus.Properties", "PropertiesChanged", response => {
                subscriptionId = response.result?.subscriptionId ?? "";
            });
    }

    // DMSService.dbusSetProperty sends numbers as doubles; asusd wants u32.
    function setProperty(name, value) {
        Quickshell.execDetached(["busctl", "--system", "set-property", busName, objectPath, iface, name, "u", String(value)]);
    }

    function setLive(value) {
        if (value !== live)
            setProperty("PlatformProfile", value);
    }

    function setForPower(ac, value) {
        setProperty(ac ? "PlatformProfileOnAc" : "PlatformProfileOnBattery", value);
        if (BatteryService.isPluggedIn === ac)
            setLive(value);
    }

    function cycle() {
        Quickshell.execDetached(["busctl", "--system", "call", busName, objectPath, iface, "NextPlatformProfile"]);
    }

    Connections {
        target: DMSService

        function onDbusSignalReceived(subscriptionId, data) {
            if (data.path === root.objectPath && data.body?.[0] === root.iface && data.body?.[1])
                root.applyProps(data.body[1]);
        }
    }

    // Picks asusd up again after it restarts or starts late.
    Timer {
        interval: 30000
        repeat: true
        running: !root.available && root.dbusReady
        onTriggered: root.refresh()
    }

    pillRightClickAction: () => root.cycle()

    horizontalBarPill: Component {
        DIcon {
            name: root.liveInfo?.icon ?? "help"
            size: root.iconSize
            color: Theme.widgetIconColor
        }
    }

    verticalBarPill: Component {
        DIcon {
            name: root.liveInfo?.icon ?? "help"
            size: root.iconSize
            color: Theme.widgetIconColor
        }
    }

    component ProfileRow: Column {
        id: row

        required property string title
        required property int selected
        required property var profiles
        property bool active: false
        signal picked(int value)

        width: parent.width
        spacing: Theme.spacingXS

        StyledText {
            text: row.title + (row.active ? " (now)" : "")
            font.pixelSize: Theme.fontSizeSmall
            font.weight: Theme.fontWeightMedium
            color: Theme.primary
        }

        DButtonGroup {
            width: parent.width
            fillWidth: true
            checkEnabled: false
            model: row.profiles.map(p => ({
                        text: p.label,
                        icon: p.icon
                    }))
            currentIndex: row.profiles.findIndex(p => p.value === row.selected)
            onSelectionChanged: (index, selected) => {
                if (selected)
                    row.picked(row.profiles[index].value);
            }
        }
    }

    popoutContent: Component {
        PopoutComponent {
            headerText: "ASUS profile"
            detailsText: root.available ? "" : "asusd is not running"

            Column {
                width: parent.width
                spacing: Theme.spacingM

                ProfileRow {
                    title: "Current"
                    selected: root.live
                    profiles: root.shownProfiles
                    onPicked: value => root.setLive(value)
                }

                ProfileRow {
                    title: "On AC"
                    selected: root.onAc
                    profiles: root.shownProfiles
                    active: BatteryService.isPluggedIn
                    onPicked: value => root.setForPower(true, value)
                }

                ProfileRow {
                    title: "On battery"
                    selected: root.onBattery
                    profiles: root.shownProfiles
                    active: !BatteryService.isPluggedIn
                    onPicked: value => root.setForPower(false, value)
                }

                StyledText {
                    width: parent.width
                    text: "Right-click the bar icon to cycle the current profile."
                    font.pixelSize: Theme.fontSizeSmall
                    color: Theme.surfaceVariantText
                    wrapMode: Text.WordWrap
                }
            }
        }
    }
}
