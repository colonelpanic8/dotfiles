import QtQuick
import qs.Common
import qs.Services
import qs.Widgets
import qs.DCommon.Widgets
import qs.Modules.Plugins

PluginComponent {
    id: root

    readonly property real cpuTemp: DgopService.cpuTemperature
    readonly property string gpuPciId: DgopService.availableGpus[0]?.pciId ?? ""
    readonly property real gpuTemp: gpuPciId ? (DgopService.widgetGpu(0, gpuPciId)?.temperature || 0) : 0
    property string heldGpuPciId: ""

    function severity(temp, warn, danger) {
        return temp > danger ? 2 : temp > warn ? 1 : 0;
    }

    function severityColor(level, fallback) {
        return [fallback, Theme.tempWarning, Theme.tempDanger][level];
    }

    readonly property int cpuSeverity: severity(cpuTemp, 69, 85)
    readonly property int gpuSeverity: severity(gpuTemp, 65, 80)

    function holdGpu() {
        if (gpuPciId === heldGpuPciId)
            return;
        if (heldGpuPciId)
            DgopService.removeGpuPciId(heldGpuPciId);
        heldGpuPciId = gpuPciId;
        if (heldGpuPciId)
            DgopService.addGpuPciId(heldGpuPciId);
    }

    onGpuPciIdChanged: holdGpu()
    Component.onCompleted: {
        DgopService.addRef(["cpu", "gpu"]);
        holdGpu();
    }
    Component.onDestruction: {
        DgopService.removeRef(["cpu", "gpu"]);
        if (heldGpuPciId)
            DgopService.removeGpuPciId(heldGpuPciId);
    }

    pillClickAction: (x, y, width, section, screen) => {
        DgopService.setSortBy("cpu");
        PopoutService.toggleProcessList(x, y, width, section, screen);
    }

    horizontalBarPill: Component {
        Row {
            spacing: Theme.spacingXS
            height: root.widgetThickness

            DIcon {
                anchors.verticalCenter: parent.verticalCenter
                name: "device_thermostat"
                size: root.iconSize
                color: root.severityColor(Math.max(root.cpuSeverity, root.gpuSeverity), Theme.widgetIconColor)
            }

            StyledText {
                anchors.verticalCenter: parent.verticalCenter
                text: root.cpuTemp > 0 ? Math.round(root.cpuTemp) + "°" : "--°"
                font.pixelSize: root.textSize
                color: root.severityColor(root.cpuSeverity, Theme.widgetTextColor)
            }

            StyledText {
                anchors.verticalCenter: parent.verticalCenter
                visible: root.gpuPciId !== ""
                text: "·"
                font.pixelSize: root.textSize
                color: Theme.surfaceVariantText
            }

            StyledText {
                anchors.verticalCenter: parent.verticalCenter
                visible: root.gpuPciId !== ""
                text: root.gpuTemp > 0 ? Math.round(root.gpuTemp) + "°" : "--°"
                font.pixelSize: root.textSize
                color: root.severityColor(root.gpuSeverity, Theme.widgetTextColor)
            }
        }
    }
}
