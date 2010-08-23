#include <hn/lib.hpp>

struct hnMain_impl
{
	template <typename t0>
	static ff::IO<void> tmp(t0 z)
	{
		t0 y = z;
		return ff::print(y);
	};
};

ff::IO<void> hnMain()
{
	typedef hnMain_impl local;
	int a = 5;
	return local::tmp(a);
};
