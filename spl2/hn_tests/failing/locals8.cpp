#include <hn/lib.hpp>

template <typename t0>
struct hnMain_impl
{
	static int f(t0 b)
	{
		int a = 1;
		return a;
	};
};

ff::IO<void> hnMain()
{
	typedef hnMain_impl<t0> local;
	return ff::print(5);
};
