include "lib";

[
  "| Count | Kind | Message |",
  "|---:|---|---|"
] + (
  .stats.top_messages[:10]
  | map("| \(.count) | \(.kind) | \(.message | md_escape) |")
)
| .[]
