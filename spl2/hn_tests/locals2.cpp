#include <hn/lib.hpp>

struct hnMain_impl
{
	static int f(int b)
	{
		int a = ff::sum(1, b);
		return ff::sum(a, b);
	};
};

ff::IO<void> hnMain()
{
	typedef hnMain_impl local;
	return ff::print(local::f(5));
};
