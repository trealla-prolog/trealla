// Tests for the device-agnostic IPv4/UDP stack. The point of the netif
// contract is that a "device" need not be a device: everything below runs
// hosted, in ordinary `make test`, with no hardware and no emulator.
//
// Frames are built here by hand, byte by byte, rather than with the stack's
// own helpers - a test that constructs packets with the code under test
// agrees with its bugs.

#include <stdio.h>
#include <string.h>

#include "net.h"

static int failures, checks;

static void ok(bool cond, const char *what)
{
	checks++;

	if (!cond) {
		failures++;
		printf("  FAIL %s\n", what);
	}
}

// --- a netif that is not a device ---------------------------------------

#define TEST_QUEUE 8

typedef struct {
	uint8_t inbox[TEST_QUEUE][NETIF_FRAME_MAX];		// waiting to be polled
	size_t inbox_len[TEST_QUEUE];
	unsigned in_head, in_count;

	uint8_t sent[TEST_QUEUE][NETIF_FRAME_MAX];		// what the stack emitted
	size_t sent_len[TEST_QUEUE];
	unsigned sent_count;
} testdev;

static bool test_send(netif *nif, const void *frame, size_t len)
{
	testdev *d = nif->device;

	if (d->sent_count == TEST_QUEUE)
		return false;

	memcpy(d->sent[d->sent_count], frame, len);
	d->sent_len[d->sent_count++] = len;
	return true;
}

static size_t test_poll(netif *nif, void *frame, size_t maxlen)
{
	testdev *d = nif->device;

	if (!d->in_count)
		return 0;

	size_t len = d->inbox_len[d->in_head];

	if (len > maxlen)
		len = maxlen;

	memcpy(frame, d->inbox[d->in_head], len);
	d->in_head = (d->in_head + 1) % TEST_QUEUE;
	d->in_count--;
	return len;
}

static bool test_link_up(netif *nif) { (void)nif; return true; }

static void inject(testdev *d, const uint8_t *frame, size_t len)
{
	unsigned slot = (d->in_head + d->in_count) % TEST_QUEUE;
	memcpy(d->inbox[slot], frame, len);
	d->inbox_len[slot] = len;
	d->in_count++;
}

// --- hand-built frames ---------------------------------------------------

static const uint8_t our_mac[6]  = {0x02, 0x00, 0x00, 0x00, 0x00, 0x01};
static const uint8_t peer_mac[6] = {0x02, 0x00, 0x00, 0x00, 0x00, 0x02};
static const uint8_t our_ip[4]   = {192, 168, 50, 2};
static const uint8_t peer_ip[4]  = {192, 168, 50, 1};
static const uint8_t mask[4]     = {255, 255, 255, 0};
static const uint8_t gw[4]       = {192, 168, 50, 1};

static void put16(uint8_t *p, uint16_t v) { p[0] = (uint8_t)(v >> 8); p[1] = (uint8_t)v; }
static uint16_t get16(const uint8_t *p) { return (uint16_t)((p[0] << 8) | p[1]); }

static size_t build_eth(uint8_t *f, const uint8_t *dst, const uint8_t *src, uint16_t type)
{
	memcpy(f, dst, 6);
	memcpy(f + 6, src, 6);
	put16(f + 12, type);
	return 14;
}

static size_t build_arp_request(uint8_t *f, const uint8_t *target_ip)
{
	static const uint8_t bcast[6] = {0xff,0xff,0xff,0xff,0xff,0xff};
	size_t n = build_eth(f, bcast, peer_mac, 0x0806);
	put16(f + n + 0, 1);
	put16(f + n + 2, 0x0800);
	f[n + 4] = 6;
	f[n + 5] = 4;
	put16(f + n + 6, 1);					// request
	memcpy(f + n + 8, peer_mac, 6);
	memcpy(f + n + 14, peer_ip, 4);
	memset(f + n + 18, 0, 6);
	memcpy(f + n + 24, target_ip, 4);
	return n + 28;
}

static size_t build_ip(uint8_t *f, uint8_t proto, size_t payload,
	const uint8_t *dst_ip)
{
	size_t n = build_eth(f, our_mac, peer_mac, 0x0800);
	uint8_t *ip = f + n;
	memset(ip, 0, 20);
	ip[0] = 0x45;
	put16(ip + 2, (uint16_t)(20 + payload));
	ip[8] = 64;
	ip[9] = proto;
	memcpy(ip + 12, peer_ip, 4);
	memcpy(ip + 16, dst_ip, 4);
	put16(ip + 10, net_checksum(ip, 20));
	return n + 20;
}

static size_t build_udp(uint8_t *f, uint16_t sport, uint16_t dport,
	const char *payload, const uint8_t *dst_ip)
{
	size_t plen = strlen(payload);
	size_t n = build_ip(f, 17, 8 + plen, dst_ip);
	put16(f + n + 0, sport);
	put16(f + n + 2, dport);
	put16(f + n + 4, (uint16_t)(8 + plen));
	put16(f + n + 6, 0);
	memcpy(f + n + 8, payload, plen);
	return n + 8 + plen;
}

static size_t build_ping(uint8_t *f, const char *payload)
{
	size_t plen = strlen(payload);
	size_t n = build_ip(f, 1, 8 + plen, our_ip);
	uint8_t *icmp = f + n;
	memset(icmp, 0, 8);
	icmp[0] = 8;							// echo request
	put16(icmp + 4, 0x1234);				// id
	put16(icmp + 6, 1);						// sequence
	memcpy(icmp + 8, payload, plen);
	put16(icmp + 2, net_checksum(icmp, 8 + plen));
	return n + 8 + plen;
}

// --- tests ---------------------------------------------------------------

static netif nif;
static testdev dev;
static net_stack net;

static void setup(void)
{
	memset(&dev, 0, sizeof(dev));
	memset(&nif, 0, sizeof(nif));
	nif.name = "test0";
	memcpy(nif.mac, our_mac, 6);
	nif.device = &dev;
	nif.link_up = test_link_up;
	nif.send = test_send;
	nif.poll = test_poll;
	ok(net_init(&net, &nif, our_ip, mask, gw), "net_init");
}

static void drain(void) { while (net_poll(&net)) ; }

static void test_checksum(void)
{
	// RFC 1071's worked example.
	static const uint8_t data[] = {0x00,0x01,0xf2,0x03,0xf4,0xf5,0xf6,0xf7};
	ok(net_checksum(data, sizeof(data)) == 0x220d, "checksum matches RFC 1071");

	// A header that already carries a correct checksum sums to zero.
	uint8_t f[64];
	size_t n = build_ip(f, 17, 0, our_ip);
	ok(net_checksum(f + 14, 20) == 0, "valid header sums to zero");
	(void)n;
}

static void test_arp(void)
{
	uint8_t f[128];
	setup();

	size_t n = build_arp_request(f, our_ip);
	inject(&dev, f, n);
	drain();

	ok(dev.sent_count == 1, "ARP request draws exactly one reply");
	const uint8_t *r = dev.sent[0];
	ok(get16(r + 12) == 0x0806, "reply is ARP");
	ok(get16(r + 14 + 6) == 2, "reply opcode is 2");
	ok(!memcmp(r, peer_mac, 6), "reply is unicast to the asker");
	ok(!memcmp(r + 14 + 8, our_mac, 6), "reply carries our MAC");
	ok(!memcmp(r + 14 + 14, our_ip, 4), "reply carries our IP");

	// An ARP request for somebody else must be ignored.
	static const uint8_t other[4] = {192, 168, 50, 99};
	dev.sent_count = 0;
	n = build_arp_request(f, other);
	inject(&dev, f, n);
	drain();
	ok(dev.sent_count == 0, "ARP for another address is ignored");
}

static void test_udp_send_needs_arp(void)
{
	uint8_t f[128];
	setup();

	ok(!net_udp_send(&net, peer_ip, 69, 1069, "x", 1),
		"send fails while the peer's MAC is unknown");
	ok(dev.sent_count == 1, "and emits one frame");
	ok(get16(dev.sent[0] + 12) == 0x0806, "which is an ARP request");
	ok(get16(dev.sent[0] + 14 + 6) == 1, "with opcode 1");

	// Teach it the peer by having the peer ask us something.
	dev.sent_count = 0;
	size_t n = build_arp_request(f, our_ip);
	inject(&dev, f, n);
	drain();

	dev.sent_count = 0;
	ok(net_udp_send(&net, peer_ip, 69, 1069, "hello", 5),
		"send succeeds once the MAC is known");
	ok(dev.sent_count == 1, "one frame out");

	const uint8_t *s = dev.sent[0];
	ok(!memcmp(s, peer_mac, 6), "addressed to the peer");
	ok(get16(s + 12) == 0x0800, "ethertype IPv4");
	ok(s[14 + 9] == 17, "protocol UDP");
	ok(net_checksum(s + 14, 20) == 0, "IPv4 header checksum is valid");
	ok(get16(s + 34 + 0) == 1069, "source port");
	ok(get16(s + 34 + 2) == 69, "destination port");
	ok(get16(s + 34 + 4) == 13, "UDP length covers header plus payload");
	ok(!memcmp(s + 42, "hello", 5), "payload intact");
}

static void test_udp_recv(void)
{
	uint8_t f[256];
	setup();
	ok(net_udp_bind(&net, 6969), "bind");
	ok(!net_udp_bind(&net, 6969), "double bind refused");

	size_t n = build_udp(f, 4321, 6969, "readings", our_ip);
	inject(&dev, f, n);
	drain();

	uint8_t from[4];
	uint16_t port = 0;
	char buf[64];
	size_t got = net_udp_recv(&net, 6969, from, &port, buf, sizeof(buf));
	ok(got == 8, "datagram length");
	ok(!memcmp(buf, "readings", 8), "datagram contents");
	ok(!memcmp(from, peer_ip, 4), "source address");
	ok(port == 4321, "source port");
	ok(net_udp_recv(&net, 6969, from, &port, buf, sizeof(buf)) == 0,
		"queue is empty afterwards");

	// A datagram for a port nobody bound goes nowhere.
	n = build_udp(f, 4321, 7000, "nope", our_ip);
	inject(&dev, f, n);
	drain();
	ok(net_udp_recv(&net, 6969, from, &port, buf, sizeof(buf)) == 0,
		"unbound port is not delivered");
}

static void test_icmp(void)
{
	uint8_t f[256];
	setup();

	// Teach it our peer first, so it can address the reply.
	size_t n = build_arp_request(f, our_ip);
	inject(&dev, f, n);
	drain();
	dev.sent_count = 0;

	n = build_ping(f, "abcdefgh");
	inject(&dev, f, n);
	drain();

	ok(dev.sent_count == 1, "ping draws one reply");
	const uint8_t *r = dev.sent[0];
	ok(r[14 + 9] == 1, "reply is ICMP");
	ok(r[34] == 0, "type is echo reply");
	ok(net_checksum(r + 34, 16) == 0, "ICMP checksum is valid");
	ok(!memcmp(r + 42, "abcdefgh", 8), "payload echoed verbatim");
	ok(net.icmp_echoes == 1, "counter");
}

static void test_malformed(void)
{
	uint8_t f[256];
	setup();
	size_t n;

	// Learn the peer so nothing below fails merely for want of an ARP entry.
	n = build_arp_request(f, our_ip);
	inject(&dev, f, n);
	drain();
	dev.sent_count = 0;

	// Each case is checked on its own: a single "something got dropped"
	// assertion passes even when five of the six are being accepted.
	unsigned dropped;

#define REJECTS(what) \
	do { \
		drain(); \
		ok(net.rx_dropped > dropped, "rejects " what); \
		ok(dev.sent_count == 0, "no reply to " what); \
		dropped = net.rx_dropped; \
	} while (0)

	dropped = net.rx_dropped;

	// Truncated: an Ethernet header and nothing else.
	inject(&dev, f, 14);
	REJECTS("a header-only frame");

	// Shorter than an Ethernet header at all.
	inject(&dev, f, 6);
	REJECTS("a runt frame");

	// Corrupt the IPv4 header checksum.
	n = build_udp(f, 1, 6969, "x", our_ip);
	f[14 + 10] ^= 0xff;
	inject(&dev, f, n);
	REJECTS("a bad header checksum");

	// Claim a total length longer than the frame.
	n = build_udp(f, 1, 6969, "x", our_ip);
	put16(f + 14 + 2, 1400);
	put16(f + 14 + 10, 0);
	put16(f + 14 + 10, net_checksum(f + 14, 20));
	inject(&dev, f, n);
	REJECTS("a length beyond the frame");

	// A fragment.
	n = build_udp(f, 1, 6969, "x", our_ip);
	put16(f + 14 + 6, 0x2000);
	put16(f + 14 + 10, 0);
	put16(f + 14 + 10, net_checksum(f + 14, 20));
	inject(&dev, f, n);
	REJECTS("a fragment");

	// An ethertype nobody handles.
	n = build_eth(f, our_mac, peer_mac, 0x88cc);
	inject(&dev, f, n + 10);
	REJECTS("an unknown ethertype");

#undef REJECTS

	// Addressed to somebody else's MAC: not ours, not broadcast.
	static const uint8_t stranger[6] = {0x02,0,0,0,0,0x09};
	n = build_eth(f, stranger, peer_mac, 0x0800);
	inject(&dev, f, n + 20);
	drain();
	ok(dev.sent_count == 0, "frames for another station are ignored");
}

static void test_routing(void)
{
	setup();
	static const uint8_t far[4] = {8, 8, 8, 8};
	ok(!net_udp_send(&net, far, 53, 1234, "q", 1), "off-subnet send defers");
	ok(dev.sent_count == 1, "and ARPs");
	// It must ARP for the gateway, not for the far address.
	ok(!memcmp(dev.sent[0] + 14 + 24, gw, 4), "ARP asks for the gateway");
}

int main(void)
{
	test_checksum();
	test_arp();
	test_udp_send_needs_arp();
	test_udp_recv();
	test_icmp();
	test_malformed();
	test_routing();

	// Silent when everything passes: the recipe line showing the binary run
	// is the report. Anything printed here is a problem.
	if (failures)
		printf("net: %d of %d checks failed\n", failures, checks);

	return failures ? 1 : 0;
}
