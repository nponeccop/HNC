#include <hn/lib.hpp>

struct hasChar_impl
{
	typedef hasChar_impl self;
	int c;

	static bool xand(bool a, bool b)
	{
		return ff::_not(ff::_not(a) || ff::_not(b));
	};
	static bool neq(int a, int b)
	{
		return ff::_not(a == b);
	};
	bool loopCond(ff::ptr<int> s)
	{
		return xand(neq(c, ff::deref(s)), neq(ff::deref(s), 0));
	};
};

bool hasChar(int c, ff::ptr<int> s)
{
	typedef hasChar_impl local;
	local impl = { c };
	ff::ptr<int> ss = s;
	while (impl.loopCond(ss))
	{
		ss = ff::next(ss);
	}
	return ff::deref(ss) == c;
};
