import json
import subprocess
import tempfile
import unittest
from pathlib import Path

ROOT = Path(__file__).resolve().parents[1]


def prompt(text="Use the browser"):
    return {"type": "user", "message": {"content": text}}


def tool(name, result, *, tab_id=None, error=False, call_id="call"):
    return [
        {
            "type": "assistant",
            "message": {
                "content": [
                    {
                        "type": "tool_use",
                        "id": call_id,
                        "name": "mcp__claude-in-chrome__" + name,
                        "input": {} if tab_id is None else {"tabId": tab_id},
                    }
                ]
            },
        },
        {
            "type": "user",
            "message": {
                "content": [
                    {
                        "type": "tool_result",
                        "tool_use_id": call_id,
                        "content": [{"type": "text", "text": result}],
                        "is_error": error,
                    }
                ]
            },
        },
    ]


def context(*tab_ids):
    return tool(
        "tabs_context_mcp",
        json.dumps(
            {
                "availableTabs": [{"tabId": tab_id} for tab_id in tab_ids],
                "tabGroupId": 77,
            }
        ),
    )


def close(tab_id, *, error=False):
    return tool(
        "tabs_close_mcp",
        "Close failed"
        if error
        else f"Closed tab {tab_id}. Group is now empty (auto-removed).",
        tab_id=tab_id,
        error=error,
    )


class BrowserCleanupTest(unittest.TestCase):
    def run_hook(self, entries, **payload):
        with tempfile.TemporaryDirectory() as directory:
            transcript = Path(directory) / "session.jsonl"
            transcript.write_text("\n".join(json.dumps(entry) for entry in entries))
            result = subprocess.run(
                [str(ROOT / "dotfiles/lib/bin/agent-browser-cleanup")],
                input=json.dumps({"transcript_path": str(transcript), **payload}),
                text=True,
                capture_output=True,
                check=True,
            )
        self.assertEqual(result.stderr, "")
        return json.loads(result.stdout) if result.stdout else None

    def test_unclosed_group_blocks_completion(self):
        result = self.run_hook([prompt(), *context(11)])
        self.assertEqual(result["decision"], "block")
        self.assertIn("createIfEmpty: false", result["reason"])

    def test_closed_group_allows_completion(self):
        self.assertIsNone(self.run_hook([prompt(), *context(11), *close(11)]))

    def test_closing_one_tab_leaves_other_tabs_pending(self):
        self.assertIsNotNone(self.run_hook([prompt(), *context(11, 12), *close(11)]))

    def test_failed_close_still_blocks(self):
        self.assertIsNotNone(
            self.run_hook([prompt(), *context(11), *close(11, error=True)])
        )

    def test_blank_created_tab_blocks(self):
        self.assertIsNotNone(
            self.run_hook(
                [
                    prompt(),
                    *tool("tabs_create_mcp", "Created new tab. Tab ID: 11"),
                ]
            )
        )

    def test_batch_tab_context_blocks(self):
        self.assertIsNotNone(
            self.run_hook(
                [
                    prompt(),
                    *tool(
                        "browser_batch",
                        '\n\nTab Context:\n- Available tabs:\n  • tabId 11: "New Tab" ("")',
                    ),
                ]
            )
        )

    def test_empty_context_allows_completion(self):
        self.assertIsNone(self.run_hook([prompt(), *context()]))

    def test_browser_inventory_does_not_require_cleanup(self):
        self.assertIsNone(
            self.run_hook(
                [
                    prompt(),
                    *tool("list_connected_browsers", '[{"name": "Browser 1"}]'),
                ]
            )
        )

    def test_failed_browser_connection_does_not_require_cleanup(self):
        self.assertIsNone(
            self.run_hook(
                [
                    prompt(),
                    *tool("tabs_context_mcp", "Browser unavailable", error=True),
                ]
            )
        )

    def test_new_prompt_does_not_reopen_old_cleanup(self):
        self.assertIsNone(
            self.run_hook([prompt(), *context(11), prompt("Unrelated task")])
        )

    def test_hook_cannot_loop_when_browser_is_unavailable(self):
        self.assertIsNone(
            self.run_hook([prompt(), *context(11)], stop_hook_active=True)
        )

    def test_non_browser_turn_allows_completion(self):
        self.assertIsNone(self.run_hook([prompt("Edit a file")]))

    def test_missing_transcript_allows_completion(self):
        self.assertIsNone(
            self.run_hook([], transcript_path="/nonexistent/session.jsonl")
        )


if __name__ == "__main__":
    unittest.main()
