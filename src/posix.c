#include <stdlib.h>
#include <stdio.h>
#include <string.h>
#include <time.h>
#include <ctype.h>
#include <math.h>
#include <float.h>
#include <errno.h>

#include "trealla.h"
#include "internal.h"
#include "query.h"

static bool fn_posix_time_1(query *q)
{
	GET_FIRST_ARG(p1,var);
	cell tmp;
	make_int(&tmp, time(NULL));
	return unify(q, p1, p1_ctx, &tmp, q->st.curr_frame);
}

builtins g_posix_bifs[] =
{
	{"posix_time", 1, fn_posix_time_1, "posix-tim(-integer)", false, false, BLAH},
	{0}
};

