#include <hn/lib.hpp>

struct monofunc_impl
{
	static bool divisable(int x, int y)
	{
		return 0 == x % y;
	};
};

bool monofunc(int xx)
{
	typedef monofunc_impl local;
	return local::divisable(xx, 7);
};
