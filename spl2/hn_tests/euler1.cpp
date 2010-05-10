#include <hn/lib.hpp>

struct hnMain_impl
{
	struct g_impl
	{
		static bool f(int xx)
		{
			return ff::_or(ff::eq(0, ff::mod(xx, 5)), ff::eq(0, ff::mod(xx, 3)));
		};
	};

	static int g(int x, int count)
	{
		typedef g_impl local;
		return ff::_if(g_impl::f(x), ff::sum(count, x), count);
	};
};

ff::IO<void> hnMain()
{
	typedef hnMain_impl local;
	return ff::print(ff::natrec(&hnMain_impl::g, 0, 999));
};
