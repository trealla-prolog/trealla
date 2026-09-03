#include "prolog.h"
#include "query.h"

// Default (empty) port builtin table. A freestanding port that exposes board
// hardware to Prolog replaces this object through PORT_BIFS_OBJECT and defines
// its own g_port_bifs, the way BIF_OS_OBJECT and NETWORK_OBJECT are selected.

builtins g_port_bifs[] =
{
	{0}
};
