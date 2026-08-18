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
trap 'rm -f "$TMPDIR/f"; rmdir "$TMPDIR"' EXIT

printf 'factum(f).\n' > "$TMPDIR/f"
cd "$TMPDIR" || exit 1

printf '"f".\nfactum(X).\n' | "$TPL" -q -f --autofail
printf '[f].\nfactum(X).\n' | "$TPL" -q -f --autofail
