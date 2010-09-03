#include <hn/lib.hpp>

struct hnMain_impl
{
	int s;

	template <typename t0>
	ff::IO<void> reply(t0 msg)
	{
		return ff::print(s);
	};
};

ff::IO<void> hnMain()
{
	typedef hnMain_impl local;
	local impl = { 5 };
	ff::IO<void> ping = impl.reply(6);
	return ping;
};
