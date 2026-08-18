#!/bin/sh

# Issue #1113: strings and their equivalent chars lists must not take
# different paths through the toplevel consult shorthand.
#
# https://github.com/trealla-prolog/trealla/issues/1113

TPL=${TPL:-./tpl}

case "$TPL" in
	/*) ;;
	*) TPL="$(pwd)/$TPL" ;;
esac

TMPDIR=$(mktemp -d "${TMPDIR:-/tmp}/trealla-test1113.XXXXXX") || exit 1
trap 'rm -f "$TMPDIR/f" "$TMPDIR/o" "$TMPDIR/fo"; rmdir "$TMPDIR"' EXIT

printf 'factum(f).\nfrom_f.\n' > "$TMPDIR/f"
printf 'from_o.\n' > "$TMPDIR/o"
printf 'from_fo.\n' > "$TMPDIR/fo"
cd "$TMPDIR" || exit 1

printf '"f".\nfactum(X).\n' | "$TPL" -q -f --autofail
printf '[f].\nfactum(X).\n' | "$TPL" -q -f --autofail

# A one-character string cannot distinguish list traversal from treating the
# packed string as one filename. These equivalent two-element terms must both
# load f and o, never the file fo.
printf '"fo".\nfrom_f.\nfrom_o.\ncurrent_predicate(from_fo/0).\n' \
	| "$TPL" -q -f --autofail
printf '[f,o].\nfrom_f.\nfrom_o.\ncurrent_predicate(from_fo/0).\n' \
	| "$TPL" -q -f --autofail

# The same must hold when the equivalent terms occur inside a consult list.
printf '["fo"].\nfrom_f.\nfrom_o.\ncurrent_predicate(from_fo/0).\n' \
	| "$TPL" -q -f --autofail
printf '[[f,o]].\nfrom_f.\nfrom_o.\ncurrent_predicate(from_fo/0).\n' \
	| "$TPL" -q -f --autofail
