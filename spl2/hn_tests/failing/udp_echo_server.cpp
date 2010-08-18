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
	ff::IO<std::string> receive = ff::udp_receive(impl.s);
	ff::IO<void> ping = ff::bind<std::string, void>(receive, hn::bind(impl, &local::reply));
	typedef hnMain_impl local;
	local impl = { ff::udp_listen(99) };
	return ff::forever(ff::bind<std::string, void>(receive, hn::bind(impl, &local::reply)));
};
