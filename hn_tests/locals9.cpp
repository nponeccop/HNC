#include <hn/lib.hpp>

struct hnMain_impl
{
	int a;

	template <typename t1>
	ff::IO<void> tmp(t1 z)
	{
		int y = a;
		return ff::print(y);
	};
};

ff::IO<void> hnMain()
{
	typedef hnMain_impl local;
	local impl = { 5 + 1 };
	return impl.tmp(7);
};
