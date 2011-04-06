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
		return ff::not(leq(c, d));
	};
	static bool geq(int c, int d)
	{
		return ff::not(lt(c, d));
	};
	static bool and(bool a, bool b)
	{
		return ff::not(ff::not(a) || ff::not(b));
	};
	static bool between(int a, int b, int c)
	{
		return and(geq(a, b), lt(b, c));
	};
	static bool isDigit(int c)
	{
		return between(48, c, 58);
	};
	template <typename t45, typename t46, typename t47>
	struct comp_impl
	{
		boost::function<t46 (t47)> f;
		boost::function<t47 (t45)> g;

		t46 h(t45 x)
		{
			return f(g(x));
		};
	};

	template <typename t45, typename t46, typename t47>
	static boost::function<t46 (t45)> comp(boost::function<t46 (t47)> f, boost::function<t47 (t45)> g)
	{
		typedef comp_impl<t45, t46, t47> local;
		local impl = { f, g };
		return hn::bind(impl, &local::h);
	};
};

bool allDigits(ff::ptr<int> ss)
{
	typedef allDigits_impl local;
	ff::ptr<int> firstFailure = ff::whileF<ff::ptr<int>>(local::comp<ff::ptr<int>, bool, int>(&local::isDigit, &ff::deref<int>), &ff::next<int>, ss);
	return ff::deref(firstFailure) == 0;
};
