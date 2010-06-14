#include <hn/lib.hpp>

template <typename t0, typename t13>
struct hnMain_impl
{
	static ff::IO<t13> f(t0 x, ff::IO<t13> y)
	{
		ff::IO<void> g = ff::voidbind(ff::print(5), ff::print("foo"));
		return ff::voidbind(ff::print(x), y);
	};
};

ff::IO<void> hnMain()
{
	typedef hnMain_impl<t0, t13> local;
	return ff::natrec(&local::f<int, void, void>, ff::print(""), 15);
};
