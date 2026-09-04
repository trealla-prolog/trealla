#pragma once

#include <stdbool.h>
#include <stddef.h>
#include <stdint.h>

// The contract between the IPv4/UDP stack and whatever moves frames for it.
// Five entry points, all polled: no interrupts, no callbacks upwards, and
// nothing above this line knows what kind of device is below it. A driver
// fills one of these in; a second NIC on the same board, or the same stack on
// a different board, is another implementation of the same functions.
//
// Frames are COPIED at this boundary, deliberately. send() takes a frame the
// stack owns and poll() fills one the stack owns, so the driver's DMA-visible
// rings stay private to the driver. A zero-copy interface would be faster and
// would also drag cache-coherency requirements out of the driver and into the
// stack - every driver would then need the stack's buffers to come from
// non-cacheable memory, and the device independence would be a fiction.
// Keeping the hardest, most device-specific problem behind this interface is
// the point of drawing the line here.

#define NETIF_MTU 1500
#define NETIF_FRAME_MAX (NETIF_MTU + 14)		// payload plus Ethernet header
#define NETIF_MAC_LEN 6

typedef struct netif_ netif;

struct netif_ {
	const char *name;						// "genet0", "loopback", ...
	uint8_t mac[NETIF_MAC_LEN];
	void *device;							// the driver's own state

	bool (*init)(netif *nif);
	bool (*link_up)(netif *nif);
	bool (*send)(netif *nif, const void *frame, size_t len);

	// Returns the length of one frame, or 0 when nothing is waiting. Never
	// blocks: the caller decides how often to ask.
	size_t (*poll)(netif *nif, void *frame, size_t maxlen);
};
