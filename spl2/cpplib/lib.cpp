#include "hn/lib.hpp"
#include <windows.h>
namespace ff {

int incr(int i)
{
	return i + 1;
}

int sum(int x, int y)
{
	return x + y;
}

bool less(int x, int y)
{
	return x < y;
}

IO<int> readnum = IO<int>(&read<int>);

void udp_reply_impl(UdpSocket & a, std::string b)
{
	puts("udp_reply_impl");
}

UdpSocket::~UdpSocket()
{
}

RaiiSocket::~RaiiSocket()
{
//	closesocket(s);
}

IO<void> udp_reply(UdpSocket & a, std::string b)
{
	return boost::bind(&udp_reply_impl, a, b);
};


void forever_impl(IO<void> x)
{
	puts("forever_impl");
	for (;;) x.value();
}

UdpSocket udp_listen(int x)
{
	puts("ff::udp_listen");
	return x;
};

void die(const char *s)
{
	puts(s);
	char buf[1024];
	FormatMessage(FORMAT_MESSAGE_FROM_SYSTEM, NULL, GetLastError(), NULL, buf, 1024, NULL);
	::puts(buf);
}

void printret(const char *msg, int ret)
{
	char buf[1024];
	FormatMessage(FORMAT_MESSAGE_FROM_SYSTEM, NULL, GetLastError(), NULL, buf, 1024, NULL);
	printf("%s: ret=0x%08X; GLE=%s\n", msg, ret, buf);
}

UdpSocket::UdpSocket (int x)
{
	puts("UdpSocket(int)");
	s = socket(AF_INET, SOCK_DGRAM, IPPROTO_UDP);
	printret("socket", s);
	sockaddr_in sa;
	memset((char *) &sa, 0, sizeof(sa));
	sa.sin_port = htons(x);
	sa.sin_family = AF_INET;
 	sa.sin_addr.S_un.S_addr = htonl(INADDR_ANY);
	puts("about to bind");
	printret("bind", ::bind(s, (const sockaddr*)&sa, sizeof(sa)));
	printf("UdpSocket(int): socket=%08X\n", s);
}

std::string udp_receive_impl(UdpSocket &s)
{
	char buf[2048];
	printf("udp_receive_impl: s.s = %08X\n", s.s);
	int r = recv(s.s, buf, 1500, 0);
	std::string ret;
	if (r > 0 && r < 1024)
	{
		ret.assign(buf, r);
	}
	else
	{
		printret("recv", r);
	}
	return ret;
}

IO<std::string> udp_receive(UdpSocket &s)
{
	return boost::bind(&udp_receive_impl, s);
}

IO<void> forever(IO<void> x)
{
	return boost::bind(&forever_impl, x);
};

RaiiSocket::RaiiSocket()
{
}

struct WinSockInit
{
	WinSockInit()
	{
		WSADATA wsa_data;
		WSAStartup(MAKEWORD(2,0), &wsa_data);
	}
};

WinSockInit init;
};

int main(int, const char*[])
{
	hnMain().value();
	return 0;
}
