#include <hn/lib.hpp>

struct hnMain_impl
{
	static int y(int x)
	{
		return ff::incr(x);
	};
	static int f(int a)
	{
		return ff::incr(a);
	};
};

ff::IO<void> hnMain()
{
	typedef hnMain_impl local;
	return ff::print(ff::sum(hnMain_impl::f(5), hnMain_impl::y(6)));
};
