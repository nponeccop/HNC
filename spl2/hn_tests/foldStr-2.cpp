#include <hn/lib.hpp>

struct foldStr_impl
{
	typedef foldStr_impl self;
	static bool neq(int a, int b)
	{
		return ff::not(a == b);
	};
	template <typename t9>
	static bool loopCond(std::pair<t9, ff::ptr<int>> loopState)
	{
		ff::ptr<int> s = ff::snd(loopState);
		return neq(ff::deref(s), 0);
	};
};

template <typename t0, typename t1, typename t2>
t2 foldStr(t0 f, t1 e, t2 s)
{
	typedef foldStr_impl local;
	return s;
};
