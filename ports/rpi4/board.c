#include <string.h>

#include "platform/platform.h"

#include "bcm2711.h"

#if RPI4_NET
#include "genet.h"
#include "net.h"
#endif

// Board bring-up that happens after the MMU is on and before main(): the
// things a device needs that Prolog should not have to ask for.
//
// Networking failing here is not fatal. A board with no cable, no link or -
// as under QEMU, which does not emulate GENET at all - no controller simply
// runs without it, and the udp_* builtins raise
// existence_error(network_interface) if a program asks.

#ifndef RPI4_IP
#define RPI4_IP {192, 168, 50, 2}
#endif
#ifndef RPI4_NETMASK
#define RPI4_NETMASK {255, 255, 255, 0}
#endif
#ifndef RPI4_GATEWAY
#define RPI4_GATEWAY {192, 168, 50, 1}
#endif

// A locally administered address, so it cannot collide with a real assignment.
// The board's own address lives in OTP and is read over the VideoCore mailbox,
// which this port does not implement yet.
#ifndef RPI4_MAC
#define RPI4_MAC {0x02, 0x00, 0x5e, 0x00, 0x53, 0x01}
#endif

#if RPI4_NET

extern bool net_stack_attach(netif *nif, const uint8_t ip[4],
	const uint8_t mask[4], const uint8_t gateway[4]);

static netif g_nif;

void rpi4_board_init(void)
{
	static const uint8_t mac[6] = RPI4_MAC;
	static const uint8_t ip[4] = RPI4_IP;
	static const uint8_t mask[4] = RPI4_NETMASK;
	static const uint8_t gateway[4] = RPI4_GATEWAY;

	if (!rpi4_genet_open(&g_nif, mac))
		return;

	net_stack_attach(&g_nif, ip, mask, gateway);
}

#else

// Networking is opt-in: `make rpi4 RPI4_NET=1`. It is not in the default
// image because QEMU's raspi4b has no GENET, and the driver's first register
// read aborts there - so the image every CI run boots must not contain it.

void rpi4_board_init(void)
{
}

#endif
