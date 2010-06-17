#include <hn/lib.hpp>

struct hnMain_impl
{
	ff::UdpSocket s;

	ff::IO<void> reply(std::string msg)
	{
		return ff::udp_reply(s, msg);
	};

	hnMain_impl() : s(ff::udp_listen(99))
	{

	}
private:
	hnMain_impl(hnMain_impl &x) : s(x.s)
	{
	};
};

ff::IO<void> hnMain()
{
	typedef hnMain_impl local;
	local impl;
	return ff::forever(ff::bind(ff::udp_receive(impl.s), hn::bind(impl, &local::reply)));
};
