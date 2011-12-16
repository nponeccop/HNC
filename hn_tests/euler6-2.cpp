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
	template <typename t7>
	static t7 id(t7 x)
	{
		return x;
	};
};

ff::IO<void> hnMain()
{
	typedef hnMain_impl local;
	return ff::print(ff::sub(local::natr(&local::id<hn::unused>), local::natr(&local::id<hn::unused>)));
};
