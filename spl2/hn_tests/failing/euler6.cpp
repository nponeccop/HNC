#include <hn/lib.hpp>

template <typename t1>
struct hnMain_impl
{
	template <typename t1>
	struct natr_impl
	{
		boost::function<int (t1)> f;

		int g(t1 x, int total)
		{
			return ff::sum(total, f(x));
		};
	};

	static int natr(boost::function<int (t1)> f)
	{
		typedef natr_impl<t1> local;
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
	typedef hnMain_impl<t1> local;
	return ff::print(ff::sub(hnMain_impl<t1>::sqr(hnMain_impl<t1>::natr(&hnMain_impl<t1>::id)), hnMain_impl<t1>::natr(&hnMain_impl<t1>::sqr)));
};
