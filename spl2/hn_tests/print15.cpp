#include <hn/lib.hpp>

struct hnMain_impl
{
	template <typename t0, typename t9>
	static ff::IO<t9> f(t0 x, ff::IO<t9> y)
	{
		ff::IO<void> g = ff::voidbind(ff::print(5), ff::print("foo"));
		return ff::voidbind(ff::print(x), y);
	};
};

ff::IO<void> hnMain()
{
	typedef hnMain_impl local;
	return ff::natrec<ff::IO<void>>(&local::f<int, void>, ff::print(""), 15);
};
