#pragma once

#include "netif.h"

// A small IPv4 stack: Ethernet framing, ARP, IPv4 without fragmentation,
// ICMP echo and UDP. No TCP, no DHCP, no DNS, no routing beyond "same subnet
// or send it to the gateway". Enough for TFTP, which is what it is for.
//
// Nothing here allocates. The stack owns fixed buffers and a fixed ARP cache,
// so it can run in a freestanding image with no heap at all.

#define NET_ARP_ENTRIES 8
#define NET_UDP_SOCKETS 4
#define NET_UDP_QUEUE 4					// datagrams held per socket
#define NET_UDP_PAYLOAD_MAX 1472		// MTU less IPv4 and UDP headers

typedef struct {
	uint8_t ip[4];
	uint8_t mac[NETIF_MAC_LEN];
	bool valid;
	uint32_t age;						// bumped on use, oldest is evicted
} net_arp_entry;

typedef struct {
	uint8_t from_ip[4];
	uint16_t from_port;
	uint16_t len;
	uint8_t data[NET_UDP_PAYLOAD_MAX];
} net_datagram;

typedef struct {
	bool bound;
	uint16_t port;
	unsigned head, count;				// ring over queue[]
	net_datagram queue[NET_UDP_QUEUE];
} net_udp_socket;

typedef struct {
	netif *nif;
	uint8_t ip[4], mask[4], gateway[4];
	net_arp_entry arp[NET_ARP_ENTRIES];
	uint32_t clock;						// monotonic tick for ARP ageing
	net_udp_socket sockets[NET_UDP_SOCKETS];
	uint8_t rx[NETIF_FRAME_MAX];
	uint8_t tx[NETIF_FRAME_MAX];

	// Counters, which are the cheapest possible diagnostics on a board with
	// no debugger attached.
	unsigned rx_frames, rx_dropped, tx_frames, arp_requests, icmp_echoes;
} net_stack;

bool net_init(net_stack *net, netif *nif,
	const uint8_t ip[4], const uint8_t mask[4], const uint8_t gateway[4]);

// Take one waiting frame from the device and act on it. Returns true if a
// frame was processed. Call it until it returns false, as often as you like.
bool net_poll(net_stack *net);

bool net_udp_bind(net_stack *net, uint16_t port);
void net_udp_close(net_stack *net, uint16_t port);

// Fails if the destination's hardware address is not yet known, having sent
// an ARP request for it: the caller retries, which is what a protocol with
// its own retransmission (TFTP) does anyway.
bool net_udp_send(net_stack *net, const uint8_t dst_ip[4], uint16_t dst_port,
	uint16_t src_port, const void *data, size_t len);

// Returns the length of the datagram copied out, or 0 if none is waiting.
size_t net_udp_recv(net_stack *net, uint16_t port, uint8_t from_ip[4],
	uint16_t *from_port, void *buf, size_t maxlen);

uint16_t net_checksum(const void *data, size_t len);
