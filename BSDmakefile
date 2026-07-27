# bmake reads this file in preference to Makefile; GNU make never looks
# at it. The Makefile uses GNU conditionals (ifeq/ifdef), $(shell) and
# := throughout for compiler and platform detection, none of which BSD
# make understands - so without this you get a cascade of syntax errors
# instead of a straight answer.

.error Trealla's Makefile requires GNU make. Use 'gmake' instead (FreeBSD: pkg install gmake, or devel/gmake).
