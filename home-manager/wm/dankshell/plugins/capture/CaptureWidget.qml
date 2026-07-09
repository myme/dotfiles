import QtQuick
import Quickshell
import Quickshell.Io
import qs.Common
import qs.Services
import qs.Widgets
import qs.Modules.Plugins

PluginComponent {
    id: root

    layerNamespacePlugin: "capture"

    // Live recording state, polled from `week status`. Shared with waybar,
    // which polls the same command.
    property bool recording: false

    // pillClickAction stays null so a plain click always opens the capture
    // drawer (also while recording, so screenshots stay available). Stopping a
    // recording is handled by the dedicated stop button that appears in the
    // expanded pill.

    Process {
        id: statusProc
        command: ["week", "status"]
        running: false
        stdout: StdioCollector {
            onStreamFinished: {
                try {
                    root.recording = JSON.parse(text).class === "recording";
                } catch (e) {
                    // ignore transient/garbled output
                }
            }
        }
    }

    function pollStatus() {
        if (!statusProc.running)
            statusProc.running = true;
    }

    Timer {
        interval: 1500
        running: true
        repeat: true
        triggeredOnStart: true
        onTriggered: root.pollStatus()
    }

    // hyprgrab takes: region | window | output
    function screenshot(target) {
        Quickshell.execDetached(["hyprgrab", target]);
        ToastService.showInfo("Screenshot: " + target);
    }

    // week takes: start <region|window|monitor> / stop
    function record(target) {
        Quickshell.execDetached(["week", "start", target]);
        ToastService.showInfo("Recording: " + target);
        pollStatus();
    }

    function stopRecording() {
        Quickshell.execDetached(["week", "stop"]);
        ToastService.showInfo("Recording stopped");
        root.recording = false; // optimistic; poll confirms
    }

    // Horizontal bar: the capture icon is always shown; while recording the
    // pill expands to the right to reveal a pulsing REC dot and a stop button.
    horizontalBarPill: Component {
        Row {
            spacing: 0

            DankIcon {
                anchors.verticalCenter: parent.verticalCenter
                name: "screenshot_monitor"
                size: Theme.iconSize
                color: Theme.surfaceText
            }

            Item {
                id: stopWrapH
                anchors.verticalCenter: parent.verticalCenter
                height: stopRowH.implicitHeight
                width: root.recording ? stopRowH.implicitWidth + Theme.spacingXS : 0
                clip: true
                opacity: root.recording ? 1 : 0

                Behavior on width {
                    NumberAnimation {
                        duration: Theme.shortDuration
                        easing.type: Theme.standardEasing
                    }
                }
                Behavior on opacity {
                    NumberAnimation {
                        duration: Theme.shortDuration
                    }
                }

                Row {
                    id: stopRowH
                    anchors.left: parent.left
                    anchors.leftMargin: Theme.spacingXS
                    anchors.verticalCenter: parent.verticalCenter
                    spacing: Theme.spacingXS

                    DankIcon {
                        anchors.verticalCenter: parent.verticalCenter
                        name: "stop_circle"
                        size: Theme.iconSize
                        color: Theme.tempDanger
                        filled: true

                        MouseArea {
                            anchors.fill: parent
                            hoverEnabled: true
                            cursorShape: Qt.PointingHandCursor
                            onClicked: root.stopRecording()
                        }
                    }
                }
            }
        }
    }

    // Vertical bar: same idea, but the pill expands downwards.
    verticalBarPill: Component {
        Column {
            spacing: 0

            DankIcon {
                anchors.horizontalCenter: parent.horizontalCenter
                name: "screenshot_monitor"
                size: Theme.iconSize
                color: Theme.surfaceText
            }

            Item {
                id: stopWrapV
                anchors.horizontalCenter: parent.horizontalCenter
                width: stopColV.implicitWidth
                height: root.recording ? stopColV.implicitHeight + Theme.spacingXS : 0
                clip: true
                opacity: root.recording ? 1 : 0

                Behavior on height {
                    NumberAnimation {
                        duration: Theme.shortDuration
                        easing.type: Theme.standardEasing
                    }
                }
                Behavior on opacity {
                    NumberAnimation {
                        duration: Theme.shortDuration
                    }
                }

                Column {
                    id: stopColV
                    anchors.top: parent.top
                    anchors.topMargin: Theme.spacingXS
                    anchors.horizontalCenter: parent.horizontalCenter
                    spacing: Theme.spacingXS

                    DankIcon {
                        anchors.horizontalCenter: parent.horizontalCenter
                        name: "stop_circle"
                        size: Theme.iconSize
                        color: Theme.tempDanger
                        filled: true

                        MouseArea {
                            anchors.fill: parent
                            hoverEnabled: true
                            cursorShape: Qt.PointingHandCursor
                            onClicked: root.stopRecording()
                        }
                    }
                }
            }
        }
    }

    popoutContent: Component {
        PopoutComponent {
            id: pop

            headerText: "Capture"
            detailsText: "Screenshot or record a region, window, or monitor"
            showCloseButton: true

            Column {
                width: parent.width
                spacing: Theme.spacingS

                StyledText {
                    text: "Screenshot"
                    font.pixelSize: Theme.fontSizeSmall
                    color: Theme.surfaceVariantText
                }

                DankButton {
                    width: parent.width
                    text: "Region"
                    iconName: "crop"
                    onClicked: {
                        root.screenshot("region");
                        pop.closePopout();
                    }
                }
                DankButton {
                    width: parent.width
                    text: "Window"
                    iconName: "crop_square"
                    onClicked: {
                        root.screenshot("window");
                        pop.closePopout();
                    }
                }
                DankButton {
                    width: parent.width
                    text: "Monitor"
                    iconName: "monitor"
                    onClicked: {
                        root.screenshot("output");
                        pop.closePopout();
                    }
                }

                Item {
                    width: 1
                    height: Theme.spacingS
                }

                StyledText {
                    text: "Record"
                    font.pixelSize: Theme.fontSizeSmall
                    color: Theme.surfaceVariantText
                }

                DankButton {
                    width: parent.width
                    text: "Region"
                    iconName: "fiber_manual_record"
                    onClicked: {
                        root.record("region");
                        pop.closePopout();
                    }
                }
                DankButton {
                    width: parent.width
                    text: "Window"
                    iconName: "fiber_manual_record"
                    onClicked: {
                        root.record("window");
                        pop.closePopout();
                    }
                }
                DankButton {
                    width: parent.width
                    text: "Monitor"
                    iconName: "fiber_manual_record"
                    onClicked: {
                        root.record("monitor");
                        pop.closePopout();
                    }
                }

                DankButton {
                    width: parent.width
                    text: root.recording ? "Stop recording" : "Not recording"
                    enabled: root.recording
                    iconName: "stop_circle"
                    backgroundColor: Theme.errorContainer
                    textColor: Theme.error
                    onClicked: {
                        root.stopRecording();
                        pop.closePopout();
                    }
                }
            }
        }
    }

    popoutWidth: 300
    popoutHeight: 460
}
