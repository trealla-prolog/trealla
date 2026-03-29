include "lib";

[
  "| Count | File |",
  "|---:|---|"
] + (
  .stats.top_error_files[:50]
  | map("| \(.count) | `\(.file | md_code_escape)` |")
)
| .[]
