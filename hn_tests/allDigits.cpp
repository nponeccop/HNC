#include <hn/lib.hpp>

struct allDigits_impl
{
	typedef allDigits_impl self;
	static bool lt(int c, int d)
	{
		return ff::less(c, d);
	};
	static bool leq(int c, int d)
	{
		return lt(c, d) || c == d;
	};
	static bool gt(int c, int d)
	{
		return ff::_not(leq(c, d));
	};
	static bool geq(int c, int d)
	{
		return ff::_not(lt(c, d));
	};
	static bool xand(bool a, bool b)
	{
		return ff::_not(ff::_not(a) || ff::_not(b));
	};
	static bool between(int a, int b, int c)
	{
		return xand(geq(a, b), lt(b, c));
	};
	static bool isDigit(int c)
	{
		return between(48, c, 58);
	};
	static bool loopCond(ff::ptr<int> s)
	{
		return isDigit(ff::deref(s));
	};
	template <typename t37>
	static ff::ptr<t37> loopBody(ff::ptr<t37> s)
	{
		return ff::next(s);
	};
};

bool allDigits(ff::ptr<int> ss)
{
	typedef allDigits_impl local;
	ff::ptr<int> firstFailure = ss;
	while (local::loopCond(firstFailure))
	{
		firstFailure = local::loopBody(firstFailure);
	}
	return ff::deref(firstFailure) == 0;
};
