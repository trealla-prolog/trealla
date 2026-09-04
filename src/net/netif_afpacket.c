#include <arpa/inet.h>
#include <stdio.h>
#include <linux/if_packet.h>
#include <net/ethernet.h>
#include <net/if.h>
#include <stdlib.h>
#include <string.h>
#include <sys/ioctl.h>
#include <sys/socket.h>
#include <unistd.h>

#include "net.h"
#include "netif_afpacket.h"

// One socket, one interface: enough for a test rig, and this is only ever a
// test rig. A real port supplies a driver instead.

static int g_sock = -1;

static bool pk_send(netif *nif, const void *frame, size_t len)
{
	(void)nif;
	return write(g_sock, frame, len) == (ssize_t)len;
}

static size_t pk_poll(netif *nif, void *frame, size_t maxlen)
{
	(void)nif;
	ssize_t n = recv(g_sock, frame, maxlen, MSG_DONTWAIT);
	return n > 0 ? (size_t)n : 0;
}

static bool pk_link_up(netif *nif) { (void)nif; return true; }

bool netif_afpacket_open(const char *dev, netif *nif)
{
	g_sock = socket(AF_PACKET, SOCK_RAW, htons(ETH_P_ALL));

	if (g_sock < 0)
		return false;

	struct ifreq ifr;
	memset(&ifr, 0, sizeof(ifr));
	snprintf(ifr.ifr_name, IFNAMSIZ, "%s", dev);

	if (ioctl(g_sock, SIOCGIFINDEX, &ifr) < 0)
		return false;

	int index = ifr.ifr_ifindex;

	if (ioctl(g_sock, SIOCGIFHWADDR, &ifr) < 0)
		return false;

	struct sockaddr_ll ll;
	memset(&ll, 0, sizeof(ll));
	ll.sll_family = AF_PACKET;
	ll.sll_protocol = htons(ETH_P_ALL);
	ll.sll_ifindex = index;

	if (bind(g_sock, (struct sockaddr*)&ll, sizeof(ll)) < 0)
		return false;

	memset(nif, 0, sizeof(*nif));
	nif->name = dev;
	memcpy(nif->mac, ifr.ifr_hwaddr.sa_data, NETIF_MAC_LEN);
	nif->link_up = pk_link_up;
	nif->send = pk_send;
	nif->poll = pk_poll;
	return true;
}
