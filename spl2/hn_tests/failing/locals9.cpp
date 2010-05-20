#include <hn/lib.hpp>

template <typename t2>
struct hnMain_impl
{
	int a;

	ff::IO<void> tmp(t2 z)
	{
		int y = a;
		return ff::print(y);
	};
};

ff::IO<void> hnMain()
{
	typedef hnMain_impl<t2> local;
	local impl = { ff::incr(5) };
	return impl.tmp(7);
};
