#include <hn/lib.hpp>

struct hnMain_impl
{
	template <typename t0>
	static int f(t0 x)
	{
		return 5;
	};
};

ff::IO<void> hnMain()
{
	typedef hnMain_impl local;
	return ff::print(local::f(5));
};
