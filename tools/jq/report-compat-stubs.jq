include "lib";

[
  "| Count | Header |",
  "|---:|---|"
] + (
  .stats.compat_stubs[:100]
  | map("| \(.count) | `<\(.header | md_code_escape)>` |")
)
| .[]
