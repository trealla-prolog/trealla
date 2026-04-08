def top_n_by_count(stream; n):
  stream
  | sort_by(.count)
  | reverse
  | .[:n];

{
  meta: {
    source_directory: .meta.source_directory,
    generated_at: .meta.generated_at
  },

  counts: {
    top_level: .meta.top_level_count,
    flattened: .meta.flattened_count,
    errors: ([.stats.by_kind[] | select(.kind == "error") | .count] | add // 0),
    warnings: ([.stats.by_kind[] | select(.kind == "warning") | .count] | add // 0),
    notes: ([.stats.by_kind[] | select(.kind == "note") | .count] | add // 0)
  },

  top_diagnostics:
    top_n_by_count(.stats.top_messages; 20),

  top_error_files:
    top_n_by_count(.stats.top_error_files; 10),
}
