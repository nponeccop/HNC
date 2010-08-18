#include <hn/lib.hpp>

struct hnMain_impl
{
	ff::UdpSocket c;

	struct tb_impl
	{
		int start_time;

		ff::IO<void> t4(int end_time)
		{
			return ff::print(ff::sub(end_time, start_time));
		};
		template <typename t20>
		ff::IO<void> t3(t20 reply)
		{
			return ff::bind<int, void>(ff::voidbind(ff::print(reply), ff::time_msec), t4);
		};
	};

	ff::IO<void> tb(int start_time)
	{
		ff::IO<std::string> ping = ff::voidbind(ff::udp_send(c, "foo"), ff::udp_receive(c));
		typedef tb_impl local;
		local impl = { start_time };
		return ff::bind<std::string, void>(ping, hn::bind(impl, &local::t3));
	};
};

ff::IO<void> hnMain()
{
	typedef hnMain_impl local;
	local impl = { ff::udp_connect("localhost", 99) };
	return ff::forever(ff::bind<int, void>(ff::time_msec, hn::bind(impl, &local::tb)));
};
