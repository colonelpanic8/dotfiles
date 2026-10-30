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
    readonly property real frequency: DgopService.cpuFrequency
    readonly property var watts: DotfilesService.state.cpuWatts
    readonly property string powerText: watts === null || watts === undefined ? "" : "  " + watts.toFixed(1) + " W"

    Component.onCompleted: DgopService.addRef(["cpu"])
    Component.onDestruction: DgopService.removeRef(["cpu"])

    horizontalBarPill: Component {
        Row {
            spacing: Theme.spacingS
            height: root.widgetThickness

            Canvas {
                id: graph
                width: root.iconSize * 2
                height: root.widgetThickness
                readonly property var samples: root.history
                readonly property color graphColor: Theme.primary
                onSamplesChanged: requestPaint()
                onGraphColorChanged: requestPaint()
                onPaint: {
                    const ctx = getContext("2d");
                    ctx.reset();
                    if (samples.length < 2)
                        return;
                    ctx.strokeStyle = graphColor;
                    ctx.lineWidth = 2;
                    ctx.beginPath();
                    for (let i = 0; i < samples.length; i++) {
                        const x = i * width / (samples.length - 1);
                        const y = height - 2 - Math.min(100, samples[i]) * (height - 4) / 100;
                        if (i === 0)
                            ctx.moveTo(x, y);
                        else
                            ctx.lineTo(x, y);
                    }
                    ctx.stroke();
                }
            }

            StyledText {
                anchors.verticalCenter: parent.verticalCenter
                text: (root.frequency / 1000).toFixed(1) + " GHz" + root.powerText
                font.pixelSize: root.textSize
                color: Theme.widgetTextColor
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
