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

    // While recording, clicking the bar pill stops the recording directly
    // (no popout). When idle, pillClickAction is null so the framework opens
    // the capture drawer as usual.
    pillClickAction: recording ? (() => root.stopRecording()) : null

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

    horizontalBarPill: Component {
        DankIcon {
            name: root.recording ? "stop_circle" : "screenshot_monitor"
            size: Theme.iconSize
            color: root.recording ? Theme.error : Theme.surfaceText
            filled: root.recording
        }
    }

    verticalBarPill: Component {
        DankIcon {
            name: root.recording ? "stop_circle" : "screenshot_monitor"
            size: Theme.iconSize
            color: root.recording ? Theme.error : Theme.surfaceText
            filled: root.recording
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
