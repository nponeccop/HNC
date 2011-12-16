#include <hn/lib.hpp>

struct foldStr_impl
{
	template <typename t5>
	static bool loopCond(std::pair<t5, ff::ptr<int>> loopState)
	{
		ff::ptr<int> s = ff::snd(loopState);
		return ff::deref(s) == 0;
	};
};

template <typename t0, typename t1, typename t2>
t2 foldStr(t0 f, t1 e, t2 s)
{
	typedef foldStr_impl local;
	return s;
};
