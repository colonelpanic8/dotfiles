import QtQuick
import Quickshell
import qs.Common
import qs.Services
import qs.Widgets
import qs.DCommon.Widgets
import qs.Modules.Plugins

PluginComponent {
    id: root

    readonly property var history: DgopService.cpuHistory
    readonly property real usage: DgopService.cpuUsage
    readonly property color usageColor: usage > 80 ? Theme.tempDanger : usage > 60 ? Theme.tempWarning : Theme.widgetTextColor
    readonly property real frequency: DgopService.cpuFrequency
    readonly property var watts: DotfilesService.state.cpuWatts
    readonly property string powerText: watts === null || watts === undefined ? "" : "  " + watts.toFixed(1) + " W"
    readonly property real rowPitch: Math.max(6, Math.floor((widgetThickness - 2) / 2))
    readonly property real stackTextSize: Math.max(7, Math.min(Theme.fontSizeMedium, Math.round(rowPitch * 0.8)))

    Component.onCompleted: DgopService.addRef(["cpu"])
    Component.onDestruction: DgopService.removeRef(["cpu"])

    horizontalBarPill: Component {
        Item {
            implicitWidth: Math.max(root.iconSize * 2.5, readout.implicitWidth + Theme.spacingXS * 2)
            implicitHeight: root.widgetThickness

            Canvas {
                id: graph
                anchors.fill: parent
                readonly property var samples: root.history
                readonly property color graphColor: Theme.primary
                onSamplesChanged: requestPaint()
                onGraphColorChanged: requestPaint()
                onWidthChanged: requestPaint()
                onPaint: {
                    const ctx = getContext("2d");
                    ctx.reset();
                    if (samples.length < 2)
                        return;
                    const yAt = sample => height - 2 - Math.min(100, sample) * (height - 4) / 100;
                    ctx.beginPath();
                    ctx.moveTo(0, height);
                    for (let i = 0; i < samples.length; i++)
                        ctx.lineTo(i * width / (samples.length - 1), yAt(samples[i]));
                    ctx.lineTo(width, height);
                    ctx.closePath();
                    ctx.fillStyle = Theme.withAlpha(graphColor, 0.25);
                    ctx.fill();
                    ctx.strokeStyle = Theme.withAlpha(graphColor, 0.7);
                    ctx.lineWidth = 2;
                    ctx.beginPath();
                    for (let i = 0; i < samples.length; i++) {
                        const x = i * width / (samples.length - 1);
                        if (i === 0)
                            ctx.moveTo(x, yAt(samples[i]));
                        else
                            ctx.lineTo(x, yAt(samples[i]));
                    }
                    ctx.stroke();
                }
            }

            Column {
                id: readout
                anchors.centerIn: parent

                NumericText {
                    anchors.horizontalCenter: parent.horizontalCenter
                    height: root.rowPitch
                    verticalAlignment: Text.AlignVCenter
                    horizontalAlignment: Text.AlignHCenter
                    text: root.usage.toFixed(0) + "%"
                    reserveText: "88%"
                    font.pixelSize: root.stackTextSize
                    font.weight: Font.Bold
                    color: root.usageColor
                }

                NumericText {
                    anchors.horizontalCenter: parent.horizontalCenter
                    height: root.rowPitch
                    verticalAlignment: Text.AlignVCenter
                    horizontalAlignment: Text.AlignHCenter
                    text: (root.frequency / 1000).toFixed(1) + " GHz"
                    reserveText: "8.8 GHz"
                    font.pixelSize: root.stackTextSize
                    color: Theme.widgetTextColor
                }
            }
        }
    }

    popoutWidth: 360
    popoutHeight: 510
    popoutContent: Component {
        PopoutComponent {
            headerText: "Desktop controls"
            detailsText: "CPU " + DgopService.cpuUsage.toFixed(0) + "% · " + (root.frequency / 1000).toFixed(2) + " GHz" + root.powerText

            Column {
                width: parent.width
                spacing: Theme.spacingS

                Repeater {
                    model: [
                        {
                            label: "Window picker",
                            command: ["hypr_shell_ui", "window", "go"]
                        },
                        {
                            label: "Run command",
                            command: ["hypr_shell_ui", "run"]
                        },
                        {
                            label: "Terminal",
                            command: ["ghostty", "--gtk-single-instance=false"]
                        },
                        {
                            label: "Toggle screensaver",
                            command: ["hypr-screensaver", "toggle"]
                        },
                        {
                            label: "Reload window manager",
                            command: ["hyprctl", "reload"]
                        },
                        {
                            label: "Restart shell",
                            command: ["systemctl", "--user", "restart", "dms.service"]
                        },
                        {
                            label: "Lock screen",
                            command: ["loginctl", "lock-session"]
                        },
                        {
                            label: "Switch to Taffybar",
                            command: ["desktop_shell_ui", "set", "taffybar"]
                        }
                    ]
                    DButton {
                        required property var modelData
                        text: modelData.label
                        width: parent.width
                        onClicked: Quickshell.execDetached(modelData.command)
                    }
                }

                DButton {
                    width: parent.width
                    text: SessionService.idleInhibited ? "Allow idle locking" : "Inhibit idle locking"
                    onClicked: SessionService.toggleIdleInhibit()
                }
            }
        }
    }
}
