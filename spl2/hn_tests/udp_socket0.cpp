#include <hn/lib.hpp>

struct hnMain_impl
{
	static ff::IO<void> f(std::string x)
	{
		return ff::print(x);
	};
};

ff::IO<void> hnMain()
{
	ff::UdpSocket x = ff::udp_connect("localhost", 99);
	typedef hnMain_impl local;
	return ff::bind(ff::udp_receive(x), &hnMain_impl::f);
};
