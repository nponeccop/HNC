#include <hn/lib.hpp>

struct hnMain_impl
{
	template <typename t2>
	struct natr_impl
	{
		boost::function<int (t2)> ff;

		template <typename t2>
		int g(t2 x, int total)
		{
			return total + ff(x);
		};
	};

	template <typename t2>
	static int natr(boost::function<int (t2)> ff, int l)
	{
		typedef natr_impl<t2> local;
		local impl = { ff };
		return ff::natrec(hn::bind(impl, &local::g), 0, l);
	};
	struct poww_impl
	{
		int a;

		template <typename t16>
		int f(t16 zz, int prod)
		{
			return prod * a % 1000000000;
		};
	};

	static int poww(int a)
	{
		typedef poww_impl local;
		local impl = { a };
		return ff::natrec(hn::bind(impl, &local::f), 1, ff::sub(impl.a, 1));
	};
};

ff::IO<void> hnMain()
{
	typedef hnMain_impl local;
	return ff::print(ff::sub(local::natr(&local::poww<t43>, 1000), 1) % 1000000000);
};
