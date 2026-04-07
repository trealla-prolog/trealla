You are an AI assistant with access to a complete “context file” containing files in this project.  Each file is included in its entirety, separated by headers of the form:

```
-------- ./path/to/file.ext --------
<file contents>
```

**Guidelines for using the context file:**

1. **Always consult the context first.**

   * Before you generate any solution, recommendation, or code snippet, search the context for relevant definitions, functions, types, and comments.
   * If you find the answer already implemented, quote or reference the exact file and section (using the header marker) rather than reinventing it.

2. **Maintain consistency with existing code.**

   * Match the project’s coding style, naming conventions, and error‐handling patterns.
   * When extending or modifying code, ensure your changes slot cleanly into the existing files.

3. **Handle missing information gracefully.**

   * If the context does not contain the needed logic or data, clearly state that you did not find it in the provided files.
   * Offer to propose a new implementation or request additional context.

4. **Be precise and concise.**

   * Provide minimal explanations that directly address the user’s request.
   * Include exact file paths and line comments when referring back to context snippets.

Whenever you receive a question or task, begin by locating the relevant section in the context file. Only after confirming whether the solution exists should you write or modify code.
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
OUT_BASE_DIR="${2:-docs/reports}"

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
    echo "---"
    echo "id: $id"
	echo "title: Porting Diagnostics $id"
    echo "type: diagnostics"
    echo "generated_at: $timestamp"
    echo "source_directory: $INPUT_DIRECTORY"
    echo "normalized_json: ./normalized.json"
    echo "report: ./report.md"
    echo "diagnostics: ./diagnostics.md"
    echo "---"
    echo
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
-------- tools/gen-compat-stubs --------
```
#!/usr/bin/env bash

COMPAT_DIR="${COMPAT_DIR:-compat}"
HEADERS_LOG="${HEADERS_LOG:-${COMPAT_DIR}/missing_headers.txt}"
MODE="${MODE:-warning}"
FORCE="${FORCE:-false}"
MAX_ITERATIONS="${MAX_ITERATIONS:-50}"

mkdir -p "$COMPAT_DIR"
mkdir -p "$(dirname "$HEADERS_LOG")"

fatal() {
    echo "[FATAL] $*" >&2
    exit 1
}
log() {
    echo "[INFO] $*" >&2
}

warn() {
    echo "[WARN] $*" >&2
}

fatal() {
    echo "[FATAL] $*" >&2
    exit 1
}

extract_headers() {
    grep -oE 'fatal error: [^:]+: No such file or directory' |
        sed -E 's/^fatal error: ([^:]+): No such file or directory$/\1/' |
        sort -u
}

macro_name() {
    local header="$1"
    printf '%s' "$header" | tr '/.-' '___' | tr '[:lower:]' '[:upper:]'
}

write_stub() {
    local header="$1"
    local out="$COMPAT_DIR/$header"
    local macro="$(macro_name "$header")"

    mkdir -p "$(dirname "$out")"

    if [[ -e "$out" && "$FORCE" != "true" ]]; then
        return 0
    fi

    {
        echo "/* Auto-generated compat stub."
        echo " * Missing during cross-compile audit: <$header>"
        echo " * DO NOT use as real implementation."
        echo " */"
        echo "#pragma once"
        echo ""
        echo "#define TPL_MISSING_HEADER__${macro}"

        case "$MODE" in
        error)
            echo "#error \"Missing compat stub hit: <$header>\""
            ;;
        warning)
            echo "#warning \"Using generated compat stub for <$header>\""
            ;;
        empty) ;;
        *)
            echo "Unknown MODE='$MODE' (expected: warning|error|empty)" >&2
            exit 1
            ;;
        esac
        echo
    } >"$out"

    printf '%s\n' "$out"
}

if [[ -n "$COMPILE_COMMAND" ]]; then
    fatal "Usage: $0 <compile-command> [args...]" >2
fi

iteration=1

while ((iteration <= MAX_ITERATIONS)); do
    log "Iteration $iteration: running compile command: $*"

    tmp_stderr="$(mktemp)"
    trap 'rm -f "$tmp_stderr"' EXIT

    "$@" 1>/dev/null 2> "$tmp_stderr"

    mapfile -t HEADERS < <(extract_headers < "$tmp_stderr")

    if (( ${#HEADERS[@]} == 0 )); then
        log "Iteration $iteration: no missing headers found, stopping"
        exit 0
    fi

    printf '%s\n' "${HEADERS[@]}" >>"$HEADERS_LOG"

    log "Iteration $iteration: found ${#HEADERS[@]} unique missing headers." >&2
    log "Iteration $iteration: wrote header list to $HEADERS_LOG" >&2

    rm -f "$tmp_stderr"
    trap - EXIT

    created=0

    for header in "${HEADERS[@]}"; do
        if out="$(write_stub "$header")"; then
            log "Iteration $iteration: created stub: $out"
            ((created += 1))
        else
            log "Iteration $iteration: stub already exists: $COMPAT_DIR/$header"
        fi
    done

    log "Iteration $iteration: created $created new stub(s)"

    if (( created == 0 )); then
        warn "Iteration $iteration: missing headers still exist, but no new stubs were created"
        warn "Stopping to avoid an infinite loop"
        exit 1
    fi

    ((iteration += 1))
done

fatal "Reached MAX_ITERATIONS=$MAX_ITERATIONS without converging"

```
