#!/bin/sh

# set -e

TPL=./tpl
#TPL=swipl

TMP=tmp.out

trap "rm -f $TMP" EXIT

failed_count=0
succeeded_count=0

for source in tests/tests/* tests/issues*/*
do
	case "$source" in
		*.pl)
			cmd="$TPL -q -g halt "
			;;
		*.sh)
			cmd="env TPL=$TPL sh"
			;;
		*)
			continue
	esac

	echo "Running $source ..."
	valgrind -q $cmd "$source" >$TMP
	diff "${source%.*}.expected" $TMP
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
