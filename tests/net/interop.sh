#!/bin/sh
# Face the IPv4/UDP stack with a real peer: one end of a veth pair is Linux,
# the other is ours. Nothing in the path is emulated.
#
# Needs Linux, root (or CAP_NET_ADMIN + CAP_NET_RAW) and python3.

set -e

BIN=./tests/net/net_interop
OURS=192.168.99.2
PEER=192.168.99.1
VETH_A=tri0
VETH_B=tri1

if [ "$(uname -s)" != Linux ]; then
	echo "interop: needs Linux (veth and AF_PACKET); skipping"
	exit 0
fi

if [ "$(id -u)" != 0 ]; then
	echo "interop: needs root for 'ip link add'; skipping" >&2
	exit 0
fi

cleanup() {
	[ -n "$STACK" ] && kill "$STACK" 2>/dev/null
	ip link del "$VETH_A" 2>/dev/null
	rm -f /tmp/interop.log
}
trap cleanup EXIT

ip link del "$VETH_A" 2>/dev/null || true
ip link add "$VETH_A" type veth peer name "$VETH_B"
ip addr add "$PEER/24" dev "$VETH_A"
ip link set "$VETH_A" up
ip link set "$VETH_B" up

"$BIN" "$VETH_B" > /tmp/interop.log 2>&1 &
STACK=$!
sleep 1

fail=0

# ARP and ICMP together: a reply only arrives if Linux accepted our ARP and
# then accepted the echo reply's checksum.
if ping -c 3 -W 2 "$OURS" > /dev/null 2>&1; then
	echo "interop: ping ok"
else
	echo "interop: FAILED - no ping replies" >&2
	fail=1
fi

# Linux must have installed our hardware address from an ARP reply we built.
if ip neigh show "$OURS" | grep -q lladdr; then
	echo "interop: arp ok"
else
	echo "interop: FAILED - Linux learned no hardware address" >&2
	fail=1
fi

# A datagram out and a datagram back, with the far end owned by Python.
if python3 - "$OURS" "$PEER" <<'PY'
import socket, sys
ours, peer = sys.argv[1], sys.argv[2]
s = socket.socket(socket.AF_INET, socket.SOCK_DGRAM)
s.settimeout(3)
s.bind((peer, 5555))
s.sendto(b"hello from linux", (ours, 6969))
try:
    data, _ = s.recvfrom(512)
except socket.timeout:
    print("no reply"); sys.exit(1)
sys.exit(0 if data == b"HELLO FROM LINUX" else 1)
PY
then
	echo "interop: udp round trip ok"
else
	echo "interop: FAILED - no UDP round trip" >&2
	fail=1
fi

[ "$fail" = 0 ] || cat /tmp/interop.log >&2
exit "$fail"
