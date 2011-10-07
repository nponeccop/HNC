#include <hn/lib.hpp>

template <typename t17>
struct foldStr_impl
{
	typedef foldStr_impl self;
	boost::function<t17 (t17, int)> f;

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
	std::pair<t17, ff::ptr<int>> loopTrans(std::pair<t17, ff::ptr<int>> loopState)
	{
		ff::ptr<int> s = ff::snd(loopState);
		t17 e = ff::fst(loopState);
		return ff::pair(f(e, ff::deref(s)), ff::next(s));
	};
};

template <typename t17>
t17 foldStr(boost::function<t17 (t17, int)> f, t17 e, ff::ptr<int> s)
{
	typedef foldStr_impl<t17> local;
	local impl = { f };
	return ff::fst(ff::whileF<std::pair<t17, ff::ptr<int>>>(&local::loopCond<t17>, hn::bind(impl, &local::loopTrans), ff::pair(e, s)));
};
