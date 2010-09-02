#include <hn/lib.hpp>

struct hnMain_impl
{
	template <typename t2>
	static ff::IO<void> f(t2 x)
	{
		return ff::print(x);
	};
};

ff::IO<void> hnMain()
{
	typedef hnMain_impl local;
	ff::UdpSocket x = ff::udp_connect("localhost", 99);
	return ff::bind<std::string, void>(ff::udp_receive(x), &local::f<std::string>);
};
