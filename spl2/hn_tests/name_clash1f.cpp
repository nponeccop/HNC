#include <hn/lib.hpp>

struct hnMain_impl
{
	static int y(int x)
	{
		return ff::incr(x);
	};
	static int f(int y)
	{
		return ff::incr(y);
	};
};

ff::IO<void> hnMain()
{
	typedef hnMain_impl local;
	return ff::print(ff::sum(hnMain_impl::f(5), hnMain_impl::y(6)));
};
