#include <hn/lib.hpp>

template <typename t18, typename t24, typename t25>
struct foldStr_impl
{
	typedef foldStr_impl self;
	boost::function<t24 (t18, t25)> f;

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
	std::pair<t24, ff::ptr<t25>> loopTrans(std::pair<t18, ff::ptr<t25>> loopState)
	{
		ff::ptr<t25> s = ff::snd(loopState);
		t18 e = ff::fst(loopState);
		return ff::pair(f(e, ff::deref(s)), ff::next(s));
	};
};

template <typename t1, typename t18, typename t2, typename t24, typename t25>
t2 foldStr(boost::function<t24 (t18, t25)> f, t1 e, t2 s)
{
	typedef foldStr_impl<t18, t24, t25> local;
	local impl = { f };
	return s;
};
