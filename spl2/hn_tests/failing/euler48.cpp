#include <hn/lib.hpp>

template <typename t2>
struct hnMain_impl
{
	template <typename t2>
	struct natr_impl
	{
		boost::function<int (t2)> ff;

		int g(t2 x, int total)
		{
			return ff::sum(total, ff(x));
		};
	};

	static int natr(boost::function<int (t2)> ff, int l)
	{
		typedef natr_impl<t2> local;
		local impl = { ff };
		return ff::natrec(hn::bind(impl, &local::g), 0, l);
	};
	template <typename t16>
	struct poww_impl
	{
		int a;

		int f(t16 zz, int prod)
		{
			return ff::mod(ff::mul(prod, a), 1000000000);
		};
	};

	static int poww(int a)
	{
		typedef poww_impl<t16> local;
		local impl = { a };
		return ff::natrec(hn::bind(impl, &local::f), 1, ff::sub(a, 1));
	};
};

ff::IO<void> hnMain()
{
	typedef hnMain_impl<t2> local;
	return ff::print(ff::mod(ff::sub(hnMain_impl<t2>::natr(&hnMain_impl<t2>::poww<t43>, 1000), 1), 1000000000));
};
