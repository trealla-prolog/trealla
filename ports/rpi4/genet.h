#pragma once

#include "netif.h"

// The BCM2711's Gigabit Ethernet, as a netif. Polled; call the stack's
// net_poll() as often as you care to receive.

bool rpi4_genet_open(netif *nif, const uint8_t mac[6]);
