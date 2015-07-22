#include <hn/lib.hpp>

template <typename t1>
struct foldStr_impl
{
	typedef foldStr_impl self;
	boost::function<t1 (t1, int)> f;

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
	std::pair<t1, ff::ptr<int>> loopTrans(std::pair<t1, ff::ptr<int>> loopState)
	{
		ff::ptr<int> s = ff::snd(loopState);
		t1 e = ff::fst(loopState);
		return ff::pair(f(e, ff::deref(s)), ff::next(s));
	};
};

template <typename t1>
std::pair<t1, ff::ptr<int>> foldStr(boost::function<t1 (t1, int)> f, t1 e, ff::ptr<int> ss)
{
	typedef foldStr_impl<t1> local;
	local impl = { f };
	std::pair<t1, ff::ptr<int>> loop = ff::pair(e, ss);
	while (local::loopCond(loop))
	{
		loop = impl.loopTrans(loop);
	}
	return loop;
};
