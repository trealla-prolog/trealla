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
