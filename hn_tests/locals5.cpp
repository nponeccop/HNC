#include <hn/lib.hpp>

struct hnMain_impl
{
	int a;

	template <typename t0>
	ff::IO<void> tmp(t0 z)
	{
		int y = a;
		return ff::print(y);
	};
};

ff::IO<void> hnMain()
{
	typedef hnMain_impl local;
	local impl = { 5 };
	return impl.tmp(7);
};
