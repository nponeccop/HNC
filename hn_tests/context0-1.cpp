#include <hn/lib.hpp>

struct foo_impl
{
	typedef foo_impl self;
	template <typename t1, typename t2>
	static t2 ap(boost::function<t2 (t1)> foo, t1 bar)
	{
		return foo(bar);
	};
	static int t4(int xx)
	{
		return xx + 1;
	};
	static int t3(int x)
	{
		return ap<int, int>(&self::t4, x);
	};
};

int foo()
{
	typedef foo_impl local;
	return local::t3(5);
};
