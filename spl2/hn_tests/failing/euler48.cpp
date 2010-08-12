#include <hn/lib.hpp>

struct hnMain_impl
{
	struct natr_impl
	{
		boost::function<int (int)> ff;

		template <typename t2>
		int g(t2 x, int total)
		{
			return total + ff(x);
		};
	};

	static int natr(boost::function<int (int)> ff, int l)
	{
		typedef natr_impl local;
		local impl = { ff };
		return ff::natrec<int>(hn::bind(impl, &local::g), 0, l);
	};
	struct poww_impl
	{
		int a;

		template <typename t15>
		int f(t15 zz, int prod)
		{
			return prod * a % 1000000000;
		};
	};

	static int poww(int a)
	{
		typedef poww_impl local;
		local impl = { a };
		return ff::natrec<int>(hn::bind(impl, &local::f), 1, ff::sub(impl.a, 1));
	};
};

ff::IO<void> hnMain()
{
	typedef hnMain_impl local;
	return ff::print(ff::sub(local::natr(&local::poww, 1000), 1) % 1000000000);
};
