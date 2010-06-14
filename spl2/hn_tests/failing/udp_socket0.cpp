#include <hn/lib.hpp>

template <typename t3>
struct hnMain_impl
{
	static ff::IO<void> f(t3 x)
	{
		return ff::print(x);
	};
};

ff::IO<void> hnMain()
{
	ff::UdpSocket x = ff::udp_connect("localhost", 99);
	typedef hnMain_impl<t3> local;
	return ff::bind(ff::udp_receive(x), &local::f<std::string>);
};
