#include <hn/lib.hpp>

struct hnMain_impl
{
	typedef hnMain_impl self;
	static int b(int x)
	{
		return x + 1;
	};
	static int a(int x)
	{
		return x + b(x);
	};
};

ff::IO<void> hnMain()
{
	typedef hnMain_impl local;
	return ff::print(local::a(5));
};
