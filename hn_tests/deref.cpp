#include <hn/lib.hpp>

struct f_impl
{
	static bool neq(int a, int b)
	{
		return ff::_not(a == b);
	};
};

bool f(ff::ptr<int> s)
{
	typedef f_impl local;
	return local::neq(ff::deref(s), 0);
};
