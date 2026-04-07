include "lib";

[
  "| Count | Kind | Message |",
  "|---:|---|---|"
] + (
  .stats.top_messages[:50]
  | map("| \(.count) | \(.kind) | \(.message | md_escape) |")
)
| .[]
