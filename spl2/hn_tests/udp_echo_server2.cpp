#include <hn/lib.hpp>

struct hnMain_impl
{
	ff::UdpSocket s;

	ff::IO<void> reply(std::string msg)
	{
		return ff::udp_reply(s, msg);
	};
};

ff::IO<void> hnMain()
{
	typedef hnMain_impl local;
	local impl = { ff::udp_listen(99) };
	return ff::forever(ff::bind<std::string, void>(ff::udp_receive(impl.s), hn::bind(impl, &local::reply)));
};
