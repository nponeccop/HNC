#include <hn/lib.hpp>

struct hnMain_impl
{
	struct natr_impl
	{
		boost::function<int (int)> f;

		int g(int x, int total)
		{
			return ff::sum(total, f(x));
		};
	};

	static int natr(boost::function<int (int)> f)
	{
		typedef natr_impl local;
		local impl = { f };
		return ff::natrec(hn::bind(impl, &local::g), 0, 100);
	};
	static int id(int x)
	{
		return ff::sum(x, 0);
	};
	static int sqr(int x)
	{
		return ff::mul(x, x);
	};
};

ff::IO<void> hnMain()
{
	typedef hnMain_impl local;
	return ff::print(ff::sub(hnMain_impl::sqr(hnMain_impl::natr(&hnMain_impl::id)), hnMain_impl::natr(&hnMain_impl::sqr)));
};
