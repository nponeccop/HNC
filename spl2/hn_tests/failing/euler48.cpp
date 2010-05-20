#include <hn/lib.hpp>

struct hnMain_impl
{
	struct natr_impl
	{
		boost::function<int (int)> ff;

		int g(int x, int total)
		{
			return ff::sum(total, ff(x));
		};
	};

	static int natr(boost::function<int (int)> ff, int l)
	{
		typedef natr_impl local;
		local impl = { ff };
		return ff::natrec(hn::bind(impl, &local::g), 0, l);
	};
	struct poww_impl
	{
		int a;

		int f(int zz, int prod)
		{
			return ff::mod(ff::mul(prod, a), 1000000000);
		};
	};

	static int poww(int a)
	{
		typedef poww_impl local;
		local impl = { a };
		return ff::natrec(hn::bind(impl, &local::f), 1, ff::sub(a, 1));
	};
};

ff::IO<void> hnMain()
{
	typedef hnMain_impl local;
	return ff::print(ff::mod(ff::sub(hnMain_impl::natr(&hnMain_impl::poww, 1000), 1), 1000000000));
};
