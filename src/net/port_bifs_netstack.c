#include <stdio.h>
#include <stdlib.h>

#include "prolog.h"
#include "query.h"

#include "net.h"
#include "netif_afpacket.h"

// The manifest for a build whose extra builtins are the network stack's.
// A real port with more than one subsystem lists them all here, the way
// ports/rpi4/port_bifs.c does.

extern builtins g_netstack_bifs[];

builtins *g_port_bif_tables[] =
{
	g_netstack_bifs,
	NULL
};

// A hosted NETSTACK build has no board to attach the stack for it, so the
// environment does: TREALLA_NETIF names the interface, TREALLA_IP,
// TREALLA_MASK and TREALLA_GATEWAY the addressing. With none of that set
// nothing is attached and the udp_* builtins raise
// existence_error(network_interface), which is the honest answer.
//
// This lives here rather than beside the netif because the netif is also
// linked by tests/net/net_interop, which owns its own stack and must not
// have one attached behind its back.

extern bool net_stack_attach(netif *nif, const uint8_t ip[4],
	const uint8_t mask[4], const uint8_t gateway[4]);

static bool dotted(const char *text, uint8_t out[4], const char *fallback)
{
	unsigned a, b, c, d;

	if (sscanf(text ? text : fallback, "%u.%u.%u.%u", &a, &b, &c, &d) != 4)
		return false;

	out[0] = (uint8_t)a; out[1] = (uint8_t)b;
	out[2] = (uint8_t)c; out[3] = (uint8_t)d;
	return true;
}

__attribute__((constructor))
static void netstack_autoattach(void)
{
	const char *dev = getenv("TREALLA_NETIF");

	if (!dev)
		return;

	static netif nif;
	uint8_t ip[4], mask[4], gw[4];

	if (!netif_afpacket_open(dev, &nif))
		return;

	if (!dotted(getenv("TREALLA_IP"), ip, "192.168.99.2")
		|| !dotted(getenv("TREALLA_MASK"), mask, "255.255.255.0")
		|| !dotted(getenv("TREALLA_GATEWAY"), gw, "192.168.99.1"))
		return;

	net_stack_attach(&nif, ip, mask, gw);
}
