include "lib";

. as $rootdoc
| (flatten) as $flat
| ($flat
   | map({
       kind,
       message,
       option: (.option // null),
       file: caret_file($ARGS.named.root),
       line: caret_line,
       column: caret_column,
       suggested_headers: suggested_headers,
       compat_stub_header: compat_stub_header,
       fixits: fixits($ARGS.named.root)
     })
   | map(select(
       (.file == null) or
       (.file | startswith("compat/") | not)
     ))
  ) as $diag
| {
    meta: {
      source_directory: $ARGS.named.input_dir,
      source_file: $ARGS.named.input_file,
      generated_at: $ARGS.named.generated_at,
      root_dir: $ARGS.named.root,
      top_level_count: ($rootdoc | length),
      flattened_count: ($flat | length)
    },

    diagnostics: $diag,

    stats: {
      by_kind:
        ($diag
         | group_by(.kind)
         | map({kind: .[0].kind, count: length})
         | sort_by(.count)
         | reverse),

      top_messages:
        ($diag
         | group_by(.kind + "|" + .message)
         | map({
             kind: .[0].kind,
             message: .[0].message,
             count: length
           })
         | sort_by(.count)
         | reverse),

      top_error_files:
        ($diag
         | map(select(.kind == "error" and .file != null))
         | group_by(.file)
         | map({
             file: .[0].file,
             count: length
           })
         | sort_by(.count)
         | reverse),

      compat_stubs:
        ($diag
         | map(select(.compat_stub_header != null))
         | map(.compat_stub_header)
         | group_by(.)
         | map({
             header: .[0],
             count: length
           })
         | sort_by(.count)
         | reverse),

      suggested_headers:
        ($diag
         | map(select(.kind == "error" and (.suggested_headers | length) > 0))
         | group_by(.message + "|" + (.suggested_headers | join(",")))
         | map({
             error: .[0].message,
             headers: .[0].suggested_headers,
             count: length
           })
         | sort_by(.count)
         | reverse),

      top_fixits:
        ($diag
         | map(.fixits[])
         | group_by((.file // "") + ":" + ((.line // 0)|tostring) + ":" + .insert)
         | map({
             file: .[0].file,
             line: .[0].line,
             insert: .[0].insert,
             count: length
           })
         | sort_by(.count)
         | reverse)
    }
  }
