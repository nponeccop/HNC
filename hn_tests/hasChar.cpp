#include <hn/lib.hpp>

struct hasChar_impl
{
	typedef hasChar_impl self;
	int c;

	static bool and(bool a, bool b)
	{
		return ff::not(ff::not(a) || ff::not(b));
	};
	static bool neq(int a, int b)
	{
		return ff::not(a == b);
	};
	bool loopCond(ff::ptr<int> s)
	{
		return and(neq(c, ff::deref(s)), neq(ff::deref(s), 0));
	};
};

bool hasChar(int c, ff::ptr<int> s)
{
	typedef hasChar_impl local;
	local impl = { c };
	ff::ptr<int> ss = ff::whileF<ff::ptr<int>>(hn::bind(impl, &local::loopCond), &ff::next<int>, s);
	return ff::deref(ss) == c;
};
