#include <hn/lib.hpp>

struct hnMain_impl
{
	template <typename t1>
	struct natr_impl
	{
		boost::function<int (int)> f;

		int g(t1 x, int total)
		{
			return total + f(x);
		};
	};

	static int natr(boost::function<int (int)> f)
	{
		typedef natr_impl<t1> local;
		local impl = { f };
		return ff::natrec<int>(hn::bind(impl, &local::g), 0, 100);
	};
	static int id(int x)
	{
		return x + 0;
	};
	static int sqr(int x)
	{
		return x * x;
	};
};

ff::IO<void> hnMain()
{
	typedef hnMain_impl local;
	return ff::print(ff::sub(local::sqr(local::natr(&local::id)), local::natr(&local::sqr)));
};
