# BSD make lands here; GNU make never does - it reads GNUmakefile first
# and ignores this file entirely. Trealla's build uses GNU conditionals
# (ifeq/ifdef), $(shell) and := for compiler and platform detection, so
# rather than fail with a cascade of syntax errors, hand the whole build
# over to GNU make.
#
# On FreeBSD: pkg install gmake (devel/gmake).

GMAKE?=	gmake

.MAIN: all

all .DEFAULT: .PHONY
	@${GMAKE} ${.TARGETS}
