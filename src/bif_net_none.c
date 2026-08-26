#include "prolog.h"

// A disabled service contributes no predicates. This keeps capability
// discovery honest: callers see an unknown predicate, not a socket API that
// exists until its first operation.

builtins g_net_bifs[] =
{
	{0}
};
