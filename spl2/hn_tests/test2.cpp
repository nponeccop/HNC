#include <hn/lib.hpp>

struct hnMain_impl
{
	static int f(int x)
	{
		return ff::incr(x);
	};
};

ff::IO<void> hnMain()
{
	typedef hnMain_impl local;
	return ff::print(local::f(2));
};
