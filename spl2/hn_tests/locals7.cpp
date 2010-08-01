#include <hn/lib.hpp>

struct hnMain_impl
{
	template <typename t0>
	static ff::IO<void> tmp(t0 aa)
	{
		t0 y = aa;
		return ff::print(y);
	};
};

ff::IO<void> hnMain()
{
	int a = 5;
	typedef hnMain_impl local;
	return local::tmp(a);
};
