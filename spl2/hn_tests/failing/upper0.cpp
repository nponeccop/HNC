#include <hn/lib.hpp>

struct foo_impl
{
	int x;

	struct bar_impl
	{
		int baz(int z)
		{
			return impl.x + z;
		};
	};

	int bar(int y)
	{
		typedef bar_impl local;
		return impl.baz(y);
	};
};

int foo(int x)
{
	typedef foo_impl local;
	local impl = { x };
	return impl.bar(5);
};
