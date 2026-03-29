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
