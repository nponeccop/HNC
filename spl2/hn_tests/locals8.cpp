#include <hn/lib.hpp>

struct hnMain_impl
{
	template <typename t0>
	static int f(t0 b)
	{
		int a = 1;
		return a;
	};
};

ff::IO<void> hnMain()
{
	typedef hnMain_impl local;
	return ff::print(5);
};
