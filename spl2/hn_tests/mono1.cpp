#include <hn/lib.hpp>

struct monofunc_impl
{
	static bool divisable(int x, int y)
	{
		return ff::eq(0, ff::mod(x, y));
	};
};

bool monofunc(int xx)
{
	typedef monofunc_impl local;
	return monofunc_impl::divisable(xx, 7);
};
