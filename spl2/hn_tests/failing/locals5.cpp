#include <hn/lib.hpp>

template <typename t0>
struct hnMain_impl
{
	int a;

	ff::IO<void> tmp(t0 z)
	{
		int y = a;
		return ff::print(y);
	};
};

ff::IO<void> hnMain()
{
	typedef hnMain_impl<t0> local;
	local impl = { 5 };
	return impl.tmp(7);
};
