#include <hn/lib.hpp>

struct hnMain_impl
{
	template <typename t0>
	static int natr(t0 f)
	{
		return 6;
	};
	template <typename t1>
	static int id(t1 x)
	{
		return 5;
	};
};

ff::IO<void> hnMain()
{
	typedef hnMain_impl local;
	return ff::print(local::natr(&local::id<t6>));
};
