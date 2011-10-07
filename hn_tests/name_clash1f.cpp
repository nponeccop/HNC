#include <hn/lib.hpp>

struct hnMain_impl
{
	static int y(int x)
	{
		return x + 1;
	};
	static int f(int y)
	{
		return y + 1;
	};
};

ff::IO<void> hnMain()
{
	typedef hnMain_impl local;
	return ff::print(local::f(5) + local::y(6));
};
