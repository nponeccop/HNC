#include <hn/lib.hpp>

struct hnMain_impl
{
	ff::UdpSocket c;

	struct tb_impl
	{
		template <typename t6>
		static ff::IO<void> t4(t6 end_time)
		{
			return ff::print(0);
		};
		template <typename t10>
		ff::IO<void> t3(t10 reply)
		{
			return ff::bind<int, void>(ff::voidbind(ff::print(reply), ff::time_msec), &local::t4<int>);
		};
	};

	template <typename t3>
	ff::IO<void> tb(t3 start_time)
	{
		typedef tb_impl local;
		ff::IO<std::string> ping = ff::udp_receive(c);
		return ff::bind<std::string, void>(ping, hn::bind(impl, &local::t3<std::string>));
	};
};

ff::IO<void> hnMain()
{
	typedef hnMain_impl local;
	local impl = { ff::udp_connect("localhost", 99) };
	return ff::forever(ff::bind<int, void>(ff::time_msec, hn::bind(impl, &local::tb<int>)));
};
