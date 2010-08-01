#include <hn/lib.hpp>

struct hnMain_impl
{
	int a;

	template <typename t0>
	ff::IO<void> tmp(t0 z)
	{
		return ff::print(a);
	};
};

ff::IO<void> hnMain()
{
	typedef hnMain_impl local;
	local impl = { 5 };
	return impl.tmp(7);
};
