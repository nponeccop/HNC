#include <hn/lib.hpp>

struct foldStr_impl
{
	static bool neq(int a, int b)
	{
		return ff::_not(a == b);
	};
};

template <typename t0, typename t1, typename t2>
t2 foldStr(t0 f, t1 e, t2 s)
{
	typedef foldStr_impl local;
	return s;
};
