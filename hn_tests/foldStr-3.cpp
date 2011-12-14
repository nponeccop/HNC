#include <hn/lib.hpp>

template <typename t17, typename t27, typename t29>
struct foldStr_impl
{
	typedef foldStr_impl self;
	boost::function<t27 (t17, t29)> f;

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
	std::pair<t27, ff::ptr<t29>> loopTrans(std::pair<t17, ff::ptr<t29>> loopState)
	{
		ff::ptr<t29> s = ff::snd(loopState);
		t17 e = ff::fst(loopState);
		return ff::pair(f(e, ff::deref(s)), ff::next(s));
	};
};

template <typename t1, typename t17, typename t2, typename t27, typename t29>
t2 foldStr(boost::function<t27 (t17, t29)> f, t1 e, t2 s)
{
	typedef foldStr_impl<t17, t27, t29> local;
	local impl = { f };
	return s;
};
