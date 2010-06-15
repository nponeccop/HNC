#include <hn/lib.hpp>

struct hnMain_impl
{
	static int f(int b)
	{
		int a = 1 + b;
		return a + b;
	};
};

ff::IO<void> hnMain()
{
	typedef hnMain_impl local;
	return ff::print(local::f(5));
};
