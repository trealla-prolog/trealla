[
  "| Metric | Value |",
  "|---|---:|",
  "| Top-level diagnostics | \(.meta.top_level_count) |",
  "| Flattened diagnostics | \(.meta.flattened_count) |",
  "| Errors | \(.stats.by_kind | map(select(.kind == "error") | .count) | add // 0) |",
  "| Warnings | \(.stats.by_kind | map(select(.kind == "warning") | .count) | add // 0) |",
  "| Notes | \(.stats.by_kind | map(select(.kind == "note") | .count) | add // 0) |"
]
| .[]
