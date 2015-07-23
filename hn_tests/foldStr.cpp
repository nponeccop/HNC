#include <hn/lib.hpp>

template <typename t35>
struct foldStr_impl
{
	typedef foldStr_impl self;
	boost::function<t35 (t35, int)> f;

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
	std::pair<t35, ff::ptr<int>> loopTrans(std::pair<t35, ff::ptr<int>> loopState)
	{
		ff::ptr<int> s = ff::snd(loopState);
		t35 e = ff::fst(loopState);
		return ff::pair(f(e, ff::deref(s)), ff::next(s));
	};
};

template <typename t35>
t35 foldStr(boost::function<t35 (t35, int)> f, t35 e, ff::ptr<int> s)
{
	typedef foldStr_impl<t35> local;
	local impl = { f };
	std::pair<t35, ff::ptr<int>> loop = ff::pair(e, s);
	while (local::loopCond(loop))
	{
		loop = impl.loopTrans(loop);
	}
	return ff::fst(loop);
};
