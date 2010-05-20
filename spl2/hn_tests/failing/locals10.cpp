#include <hn/lib.hpp>

template <typename t0>
struct hnMain_impl
{
	int a;

	ff::IO<void> tmp(t0 z)
	{
		return ff::print(a);
	};
};

ff::IO<void> hnMain()
{
	typedef hnMain_impl<t0> local;
	local impl = { 5 };
	return impl.tmp(7);
};
