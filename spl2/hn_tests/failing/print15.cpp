#include <hn/lib.hpp>

struct hnMain_impl
{
	static ff::IO<void> f(int x, ff::IO<void> y)
	{
		ff::IO<void> g = ff::voidbind(ff::print(5), ff::print("foo"));
		return ff::voidbind(ff::print(x), y);
	};
};

ff::IO<void> hnMain()
{
	typedef hnMain_impl local;
	return ff::natrec(&hnMain_impl::f, ff::print(""), 15);
};
