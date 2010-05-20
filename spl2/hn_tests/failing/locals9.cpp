#include <hn/lib.hpp>

struct hnMain_impl
{
	int a;

	ff::IO<void> tmp(int z)
	{
		int y = a;
		return ff::print(y);
	};
};

ff::IO<void> hnMain()
{
	typedef hnMain_impl local;
	local impl = { ff::incr(5) };
	return impl.tmp(7);
};
