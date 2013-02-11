#include <hn/lib.hpp>

struct isDigit_impl
{
	typedef isDigit_impl self;
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
	static bool and(bool a, bool b)
	{
		return ff::_not(ff::_not(a) || ff::_not(b));
	};
	static bool between(int a, int b, int c)
	{
		return and(geq(a, b), lt(b, c));
	};
};

bool isDigit(int c)
{
	typedef isDigit_impl local;
	return local::between(48, c, 58);
};
