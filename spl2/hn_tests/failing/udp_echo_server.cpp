#include <hn/lib.hpp>

struct hnMain_impl
{
	typedef hnMain_impl self;
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
	ff::IO<std::string> receive = ff::udp_receive(s);
	ff::IO<void> ping = ff::bind<std::string, void>(receive, hn::bind(*this, &self::reply));
	return ff::forever(ff::bind<std::string, void>(receive, hn::bind(impl, &local::reply)));
};
