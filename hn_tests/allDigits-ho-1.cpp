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
	template <typename t33, typename t34, typename t35>
	struct comp_impl
	{
		boost::function<t34 (t35)> f;
		boost::function<t35 (t33)> g;

		t34 h(t33 x)
		{
			return f(g(x));
		};
	};

	template <typename t33, typename t34, typename t35>
	static boost::function<t34 (t33)> comp(boost::function<t34 (t35)> f, boost::function<t35 (t33)> g)
	{
		typedef comp_impl<t33, t34, t35> local;
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
