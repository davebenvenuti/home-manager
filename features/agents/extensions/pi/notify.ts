/**
 * Pi Notification Extension
 *
 * Sends a macOS native notification via terminal-notifier when Pi finishes
 * a task and is waiting for user input. Mirrors the Claude Code notification
 * hook behavior.
 *
 * Only notifies when the terminal is not focused, to avoid spamming
 * notifications while you're actively watching.
 */

import type { ExtensionAPI } from "@mariozechner/pi-coding-agent";
import { execFile } from "node:child_process";

function isTerminalFocused(): Promise<boolean> {
  return new Promise((resolve) => {
    execFile(
      "osascript",
      [
        "-e",
        'tell application "System Events" to get name of first application process whose frontmost is true',
      ],
      (err, stdout) => {
        if (err) {
          resolve(false);
          return;
        }
        const frontApp = stdout.trim().toLowerCase();
        resolve(frontApp === "ghostty" || frontApp === "terminal" || frontApp === "iterm2");
      },
    );
  });
}

function sendNotification(message: string): void {
  execFile("terminal-notifier", [
    "-message",
    message,
    "-title",
    "Pi",
    "-activate",
    "com.mitchellh.ghostty",
  ]);
}

export default function (pi: ExtensionAPI) {
  pi.on("agent_end", async () => {
    const focused = await isTerminalFocused();
    if (!focused) {
      sendNotification("Ready for input");
    }
  });
}
