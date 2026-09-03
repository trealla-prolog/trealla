#include "prolog.h"
#include "query.h"

// The default: no extra builtin tables at all. A port or optional subsystem
// replaces this object through PORT_BIFS_OBJECT and supplies its own array,
// the way BIF_OS_OBJECT and NETWORK_OBJECT are selected.

builtins *g_port_bif_tables[] =
{
	NULL
};
