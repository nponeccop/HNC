#include <hn/lib.hpp>

struct foo_impl
{
	typedef foo_impl self;
	int y;

	template <typename t2, typename t3>
	static t3 ap(boost::function<t3 (t2)> foo, t2 bar)
	{
		return foo(bar);
	};
	int t4(int x)
	{
		return x + y;
	};
	int t3(int x)
	{
		return ap<int, int>(hn::bind(*this, &self::t4), x);
	};
};

int foo(int y)
{
	typedef foo_impl local;
	local impl = { y };
	return impl.t3(5);
};
