// A netif backed by a real Linux AF_PACKET socket, so the stack faces an
// implementation nobody here wrote: Linux's own.
//
// tests/net/net_test.c proves the stack agrees with itself - every frame it
// sees there was written by the same hand that wrote the parser. This proves
// something different and harder: that Linux accepts our ARP replies, answers
// our ICMP echo replies, and delivers a UDP datagram we addressed. Run by
// tests/net/interop.sh, which supplies the veth pair.
//
// Linux only, and needs CAP_NET_RAW. Not built by a normal make.

#include <stdio.h>
#include <string.h>
#include <unistd.h>

#include "net.h"
#include "netif_afpacket.h"

int main(int argc, char **argv)
{
	const char *dev = argc > 1 ? argv[1] : "tri1";
	netif nif;

	if (!netif_afpacket_open(dev, &nif)) {
		perror("netif_afpacket_open");
		return 1;
	}

	static net_stack net;
	static const uint8_t ip[4]   = {192,168,99,2};
	static const uint8_t mask[4] = {255,255,255,0};
	static const uint8_t gw[4]   = {192,168,99,1};

	if (!net_init(&net, &nif, ip, mask, gw)) {
		printf("net_init failed\n");
		return 1;
	}

	net_udp_bind(&net, 6969);
	printf("stack up on %s as 192.168.99.2\n", dev);
	fflush(stdout);

	for (;;) {
		while (net_poll(&net)) ;

		uint8_t from[4];
		uint16_t port;
		char buf[512];
		size_t n = net_udp_recv(&net, 6969, from, &port, buf, sizeof(buf));

		if (n) {
			// Uppercase and return it, so the far end can check the round trip.
			for (size_t i = 0; i < n; i++)
				if ((buf[i] >= 'a') && (buf[i] <= 'z')) buf[i] -= 32;

			net_udp_send(&net, from, port, 6969, buf, n);
		}

		usleep(2000);
	}
}
