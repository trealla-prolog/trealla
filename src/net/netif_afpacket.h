#pragma once

#include "netif.h"

// A netif over a Linux AF_PACKET socket. Linux only, and needs CAP_NET_RAW.
// This is not a driver: it is how the stack is faced with a real peer without
// a board, both in tests/net/interop.sh and in a NETSTACK=1 build.

bool netif_afpacket_open(const char *dev, netif *nif);
