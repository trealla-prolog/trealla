#!/bin/sh

TPL=./tpl

TMP=tmp.out

trap "rm -f $TMP" EXIT

failed_count=0
succeeded_count=0

for source in tests/tests/* tests/issues*/*

do
	case "$source" in
		*.pl)
			cmd="$TPL -q -f -g halt "
			;;
		*.sh)
			cmd="env TPL=$TPL bash"
			;;
		*)
			continue
	esac

	echo "Running $source ..."
	timeout 30 $cmd "$source" >$TMP
	diff -a --strip-trailing-cr "${source%.*}.expected" $TMP
	if [ $? -eq 0 ]
	then
		succeeded_count=$(expr $succeeded_count + 1)
	else
		failed_count=$(expr $failed_count + 1)
	fi
done

cat <<EOF

============
TEST SUMMARY
============
Failed: $failed_count
Succeeded: $succeeded_count
EOF
