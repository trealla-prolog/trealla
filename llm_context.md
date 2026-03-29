-------- tools/create-report --------
```
#!/usr/bin/env bash

jq-with-lib() {
    jq -L tools/jq "$@"
}

fatal() {
    echo "[FATAL] $@" >&2
    exit 1
}

INPUT_DIRECTORY=${1:?usage: $0 <build-dir-with-json-logs> [out-dir]}
OUT_BASE_DIR="${2:-reports}"

shopt -s nullglob
json_files=("${INPUT_DIRECTORY}"/*.gcc.json)
shopt -u nullglob

if ((${#json_files[@]} == 0)); then
    echo "error: no *.gcc.json files found in '${INPUT_DIRECTORY}'" >&2
    exit 1
fi

build_log="$(mktemp)"
trap 'rm -f "$build_log"' EXIT

jq -s 'map(if type == "array" then . else [.] end) | add' \
    "${json_files[@]}" \
    >"$build_log" || fatal "failed to combine gcc logs"

timestamp="$(date +%Y-%m-%d_%H-%M-%S)"
OUT_DIR="${OUT_BASE_DIR}/${timestamp}"
mkdir -p "$OUT_DIR"

NORMALIZED_JSON="$OUT_DIR/normalized.json"
REPORT_MD="$OUT_DIR/report.md"
DIAGNOSTICS_MD="$OUT_DIR/diagnostics.md"

ROOT_DIR="$(pwd)"

jq-with-lib \
    --arg input_dir "$INPUT_DIRECTORY" \
    --arg input_file "$build_log" \
    --arg generated_at "$timestamp" \
    --arg root "$ROOT_DIR" \
    -f tools/jq/normalize.jq \
    "$build_log" >"$NORMALIZED_JSON" || fatal "failed to normalize json"

report_count=$(
  find "$OUT_BASE_DIR" -mindepth 1 -maxdepth 1 -type d -printf '%f\n' 2>/dev/null \
      | wc -l
)

if [[ -z "${report_count:-}" ]]; then
  id=1
else
  id=$((report_count))
fi

{
    echo "# Porting Diagnostics Report $id"
    echo
    echo "> Auto-generated. Do not edit manually."
    echo
	echo "- Corresponding diagnostics: [diagnostics.md](./diagnostics.md)"
    echo "- Source directory: \`$INPUT_DIRECTORY\`"
    echo "- Generated at: \`$timestamp\`"
    echo

    echo "## Overview"
    echo
    jq-with-lib -r -f tools/jq/report-overview.jq "$NORMALIZED_JSON" || fatal "failed to create overview section"

    echo
    echo "## Top diagnostics"
    echo
    jq-with-lib -r -f tools/jq/report-top-diagnostics.jq "$NORMALIZED_JSON" || fatal "failed to create top-diagnostics section"

    echo
    echo "## Top files with errors"
    echo
    jq-with-lib -r -f tools/jq/report-top-error-files.jq "$NORMALIZED_JSON" || fatal "failed to create top-files section"

    echo
    echo "## Compat stubs used"
    echo
    jq-with-lib -r -f tools/jq/report-compat-stubs.jq "$NORMALIZED_JSON" || fatal "failed to create compat-stubs section"

    echo
    echo "## First 100 unique errors"
    echo
    jq-with-lib -r -f tools/jq/report-unique-errors.jq "$NORMALIZED_JSON" || fatal "failed to create first-100 section"

} >"$REPORT_MD" || fatal "failed to create md report"

{
	echo "# Porting Diagnostics $id"
	echo
    echo "> Auto-generated. Do not edit manually."
    echo
	echo "- Corresponding report: [report.md](./report.md)"
    echo "- Source directory: \`$INPUT_DIRECTORY\`"
    echo "- Generated at: \`$timestamp\`"
	echo

	jq-with-lib -r -f tools/jq/report-diagnostics-by-file.jq "$NORMALIZED_JSON" || fatal "failed to create diagnostics"

} >"$DIAGNOSTICS_MD" || fatal "failed to create md diagnostics"

if command -v prettier >/dev/null 2>&1; then
    echo "Formatting report using 'prettier -w $REPORT_MD"
    prettier -w "$REPORT_MD"
    prettier -w "$DIAGNOSTICS_MD"
elif command -v npx >/dev/null 2>&1; then
    echo "Formatting using 'npx prettier -w $REPORT_MD"
    npx prettier -w "$REPORT_MD"
    npx prettier -w "$DIAGNOSTICS_MD"
elif command -v bunx >/dev/null 2>&1; then
    echo "Formatting using 'npx prettier -w $REPORT_MD"
    bunx prettier -w "$REPORT_MD"
    bunx prettier -w "$DIAGNOSTICS_MD"
else
    echo "Unable to format report, can't find 'prettier', 'npx' or 'bunx' commands"
fi

LATEST_LINK="${OUT_BASE_DIR}/latest"

ln -sfn "$(realpath --relative-to="$OUT_BASE_DIR" "$OUT_DIR")" "$LATEST_LINK" \
  || fatal "failed to update latest symlink"

echo "Wrote:"
echo "  $NORMALIZED_JSON"
echo "  $REPORT_MD"
echo "  $LATEST_LINK -> $OUT_DIR"
```
-------- tools/jq/report-diagnostics-by-file.jq --------
```jq
include "lib";

.diagnostics
| sort_by(.file // "", .line // 0)
| group_by(.file // "n/a")
| map({
    file: (.[0].file // "n/a"),
    anchor: ((.[0].file // "n/a") | md_anchor),
    diagnostics: .
})
| (
    [
      "# Diagnostics grouped by file",
      "",
      "## Table of contents",
      ""
    ]
    + (
      map("- [`" + (.file | md_code_escape) + "`](#" + .anchor + ")")
    )
    + [
      ""
    ]
    + (
      map(
        "<a id=\"" + .anchor + "\"></a>\n"
        + "## `" + (.file | md_code_escape) + "`\n\n"
        + (
            .diagnostics
            | map(
                "- **[" + (.kind // "unknown") + "]** "
                + (.message | md_escape)
                + "\n"
                + (
                    if .line != null then
                      "  - Line: " + (.line | tostring) + "\n"
                    else
                      ""
                    end
                  )
                + (
                    if (.suggested_headers | length) > 0 then
                      "  - Suggested headers: "
                      + (.suggested_headers | map("`<" + (. | md_code_escape) + ">`") | join(", "))
                      + "\n"
                    else
                      ""
                    end
                  )
              )
            | join("\n")
          )
        + "\n\n---\n"
      )
    )
  )
| .[]
```
-------- tools/jq/report-unique-errors.jq --------
```jq
include "lib";

.diagnostics
| map(select(.kind == "error"))
| unique_by(.message + "|" + (.file // "") + "|" + ((.line // 0)|tostring))
| .[:100]
| map(
    "**" + .message + "**\n\n"
    + "\t- File: `" + ((.file // "n/a") | md_code_escape) + "`\n"
    + "\t- Line: " + ((.line // "n/a")|tostring) + "\n"
    + (
        if (.suggested_headers | length) > 0
        then "\t- Suggested headers: " + (.suggested_headers | map("`<" + (. | md_code_escape) + ">`") | join(", ")) + "\n"
        else ""
        end
      )
	+ "---"
  )
| .[]
```
-------- tools/jq/report-compat-stubs.jq --------
```jq
include "lib";

[
  "| Count | Header |",
  "|---:|---|"
] + (
  .stats.compat_stubs[:100]
  | map("| \(.count) | `<\(.header | md_code_escape)>` |")
)
| .[]
```
-------- tools/jq/report-top-error-files.jq --------
```jq
include "lib";

[
  "| Count | File |",
  "|---:|---|"
] + (
  .stats.top_error_files[:50]
  | map("| \(.count) | `\(.file | md_code_escape)` |")
)
| .[]
```
-------- tools/jq/report-top-diagnostics.jq --------
```jq
include "lib";

[
  "| Count | Kind | Message |",
  "|---:|---|---|"
] + (
  .stats.top_messages[:50]
  | map("| \(.count) | \(.kind) | \(.message | md_escape) |")
)
| .[]
```
-------- tools/jq/report-overview.jq --------
```jq
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
```
-------- tools/jq/normalize.jq --------
```jq
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
```
-------- tools/jq/lib.jq --------
```jq
def flatten:
  [ .[] as $d | [$d] + ($d.children // []) ] | add;

def normalize_path($root; $p):
  if ($p == null) then null
  elif ($p | startswith($root + "/")) then $p[($root|length + 1):]
  else $p
  end;

def caret_file($root):
  normalize_path($root; .locations[0].caret.file // null);

def caret_line:
  .locations[0].caret.line // null;

def caret_column:
  .locations[0].caret.column // null;

def suggested_headers:
  [
    (.children // [])[]
    | select(.kind == "note")
    | .message
    | capture("header .<(?<hdr>[^>]+)>.")?.hdr
  ]
  | map(select(. != null))
  | unique;

def compat_stub_header:
  if (.kind == "warning") and (.message | startswith("#warning \"Using generated compat stub for <")) then
    (.message | capture("Using generated compat stub for <(?<hdr>[^>]+)>") | .hdr)
  else
    null
  end;

def fixits($root):
  [
    (.fixits // [])[]
    | {
        file: normalize_path($root; .start.file // null),
        line: (.start.line // null),
        column: (.start.column // null),
        insert: (.string // "")
      }
  ];

def md_escape:
  gsub("\\\\"; "\\\\\\\\")
  | gsub("\\|"; "\\|")
  | gsub("\\*"; "\\*")
  | gsub("`"; "\\`")
  | gsub(">"; "\\>")
  | gsub("<"; "\\<")
  | gsub("_"; "\\_");

def md_code_escape:
  gsub("`"; "\\`");

def md_code_escape_multiline:
  gsub("`"; "\\\\`")
  | gsub("_"; "\\\\_")
  | gsub("\n"; "\\\\n");

def md_anchor:
  ascii_downcase
  | gsub("[^a-z0-9._/-]+"; "-")
  | gsub("^-+"; "")
  | gsub("-+$"; "");
```
