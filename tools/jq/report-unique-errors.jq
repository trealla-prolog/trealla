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
