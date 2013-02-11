#include <hn/lib.hpp>

template <typename t29>
struct foldStr_impl
{
	typedef foldStr_impl self;
	boost::function<t29 (t29, int)> f;

	static bool neq(int a, int b)
	{
		return ff::_not(a == b);
	};
	template <typename t9>
	static bool loopCond(std::pair<t9, ff::ptr<int>> loopState)
	{
		ff::ptr<int> s = ff::snd(loopState);
		return neq(ff::deref(s), 0);
	};
	std::pair<t29, ff::ptr<int>> loopTrans(std::pair<t29, ff::ptr<int>> loopState)
	{
		ff::ptr<int> s = ff::snd(loopState);
		t29 e = ff::fst(loopState);
		return ff::pair(f(e, ff::deref(s)), ff::next(s));
	};
};

template <typename t29>
t29 foldStr(boost::function<t29 (t29, int)> f, t29 e, ff::ptr<int> s)
{
	typedef foldStr_impl<t29> local;
	local impl = { f };
	return ff::fst(ff::whileF<std::pair<t29, ff::ptr<int>>>(&local::loopCond<t29>, hn::bind(impl, &local::loopTrans), ff::pair(e, s)));
};
