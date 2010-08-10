#include <hn/lib.hpp>

struct hnMain_impl
{
	struct natr_impl
	{
		template <typename t1, typename t2>
		static int g(t1 x, t2 total)
		{
			return 1;
		};
	};

	template <typename t0>
	static int natr(t0 f)
	{
		typedef natr_impl local;
		return ff::natrec<int>(&local::g<int, int>, 0, 100);
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
