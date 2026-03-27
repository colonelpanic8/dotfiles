#!/usr/bin/env python3
"""
Mine external feedback from recent GitHub PRs.

Examples:
  python scripts/mine_pr_feedback.py --repo NixOS/nixpkgs --author colonelpanic8
  python scripts/mine_pr_feedback.py --repo NixOS/nixpkgs --author colonelpanic8 --limit 30 --format json
"""

from __future__ import annotations

import argparse
import json
import subprocess
import sys
from collections import Counter
from concurrent.futures import ThreadPoolExecutor, as_completed


def run(cmd: list[str]) -> str:
    proc = subprocess.run(cmd, capture_output=True, text=True)
    if proc.returncode != 0:
        raise RuntimeError(proc.stderr.strip() or f"command failed: {' '.join(cmd)}")
    return proc.stdout


def gh_json(args: list[str]) -> object:
    return json.loads(run(["gh", *args]))


def fetch_prs(repo: str, author: str, limit: int) -> list[dict]:
    prs: dict[int, dict] = {}
    for state in ("open", "closed"):
        data = gh_json(
            [
                "search",
                "prs",
                "--repo",
                repo,
                "--author",
                author,
                "--limit",
                str(max(limit, 30)),
                "--state",
                state,
                "--json",
                "number,title,state,closedAt,updatedAt,url",
            ]
        )
        for pr in data:
            prs[pr["number"]] = pr
    return sorted(
        prs.values(),
        key=lambda pr: (pr["updatedAt"], pr["number"]),
        reverse=True,
    )[:limit]


def fetch_feedback(repo: str, author: str, pr: dict) -> dict:
    owner, name = repo.split("/", 1)
    number = pr["number"]

    def api(path: str) -> list[dict]:
        return gh_json(["api", f"repos/{owner}/{name}/{path}", "--paginate"])

    issue_comments = api(f"issues/{number}/comments")
    review_comments = api(f"pulls/{number}/comments")
    reviews = api(f"pulls/{number}/reviews")

    comments = []
    for comment in issue_comments:
        login = comment["user"]["login"]
        body = (comment.get("body") or "").strip()
        if login != author and body:
            comments.append({"kind": "issue", "user": login, "body": body})
    for comment in review_comments:
        login = comment["user"]["login"]
        body = (comment.get("body") or "").strip()
        if login != author and body:
            comments.append(
                {
                    "kind": "review_comment",
                    "user": login,
                    "body": body,
                    "path": comment.get("path"),
                    "line": comment.get("line"),
                }
            )
    for review in reviews:
        login = review["user"]["login"]
        body = (review.get("body") or "").strip()
        if login != author and body:
            comments.append(
                {
                    "kind": "review",
                    "user": login,
                    "body": body,
                    "state": review.get("state"),
                }
            )

    return {**pr, "comments": comments}


def is_bot(login: str) -> bool:
    return login.endswith("[bot]") or login in {"github-actions", "app/dependabot"}


def render_markdown(results: list[dict], include_bots: bool) -> str:
    commenters = Counter()
    kept = []
    for pr in results:
        comments = [
            comment
            for comment in pr["comments"]
            if include_bots or not is_bot(comment["user"])
        ]
        if comments:
            kept.append({**pr, "comments": comments})
            commenters.update(comment["user"] for comment in comments)

    lines = [
        "# PR Feedback Summary",
        "",
        f"- PRs scanned: {len(results)}",
        f"- PRs with external feedback: {len(kept)}",
        "",
        "## Top commenters",
        "",
    ]
    for user, count in commenters.most_common(10):
        lines.append(f"- `{user}`: {count}")

    for pr in kept:
        lines.extend(
            [
                "",
                f"## PR #{pr['number']}: {pr['title']}",
                "",
                f"- URL: {pr['url']}",
                f"- State: {pr['state']}",
                "",
            ]
        )
        for comment in pr["comments"]:
            body = comment["body"].replace("\r", " ").replace("\n", " ").strip()
            snippet = body[:280] + ("..." if len(body) > 280 else "")
            lines.append(f"- `{comment['user']}` `{comment['kind']}`: {snippet}")

    return "\n".join(lines) + "\n"


def main() -> int:
    parser = argparse.ArgumentParser(description="Collect review feedback from recent GitHub PRs.")
    parser.add_argument("--repo", required=True, help="GitHub repo in owner/name form")
    parser.add_argument("--author", required=True, help="PR author to inspect")
    parser.add_argument("--limit", type=int, default=20, help="How many recent PRs to inspect")
    parser.add_argument(
        "--format",
        choices=("markdown", "json"),
        default="markdown",
        help="Output format",
    )
    parser.add_argument(
        "--include-bots",
        action="store_true",
        help="Keep bot comments in the output",
    )
    parser.add_argument(
        "--workers",
        type=int,
        default=6,
        help="Maximum concurrent GitHub API workers",
    )
    args = parser.parse_args()

    try:
        run(["gh", "auth", "status"])
    except RuntimeError as err:
        print(err, file=sys.stderr)
        return 1

    prs = fetch_prs(args.repo, args.author, args.limit)

    results = []
    with ThreadPoolExecutor(max_workers=args.workers) as pool:
        futures = [pool.submit(fetch_feedback, args.repo, args.author, pr) for pr in prs]
        for future in as_completed(futures):
            results.append(future.result())

    results.sort(key=lambda pr: (pr["updatedAt"], pr["number"]), reverse=True)

    if args.format == "json":
        if not args.include_bots:
            for pr in results:
                pr["comments"] = [
                    comment for comment in pr["comments"] if not is_bot(comment["user"])
                ]
        json.dump(results, sys.stdout, indent=2)
        sys.stdout.write("\n")
    else:
        sys.stdout.write(render_markdown(results, args.include_bots))

    return 0


if __name__ == "__main__":
    raise SystemExit(main())
