---
title: "Reports"
layout: default
permalink: /reports/
---

# Reports

{% assign seen = "" | split: "" %}
{% assign reports = site.pages
  | where: "type", "report"
  | sort: "id"
  | reverse %}

{% if reports.size == 0 %}
_No reports found._
{% endif %}

{% for r in reports %}
{% unless seen contains r.id %}
{% assign seen = seen | push: r.id %}

### Report {{ r.id }}

**{{ r.counts.errors }} errors** ·
{{ r.counts.warnings }} warnings ·
{{ r.counts.notes }} notes

[View Report]({{ r.permalink | relative_url }}) ·
[Full Diagnostics]({{ r.diagnostics_permalink | relative_url }}) ·
[JSON]({{ r.normalized_json | relative_url }})

_Generated at {{ r.generated_at }}_

{% endunless %}
{% endfor %}
