#include <hn/lib.hpp>

struct hnMain_impl
{
	template <typename t3>
	static ff::IO<void> f(t3 x)
	{
		return ff::print(x);
	};
};

ff::IO<void> hnMain()
{
	ff::UdpSocket x = ff::udp_connect("localhost", 99);
	typedef hnMain_impl local;
	return ff::bind<std::string, void>(ff::udp_receive(x), &local::f<std::string>);
};
