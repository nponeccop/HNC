#include <hn/lib.hpp>

struct hnMain_impl
{
	struct g_impl
	{
		static bool f(int xx)
		{
			return 0 == xx % 5 || 0 == xx % 3;
		};
	};

	static int g(int x, int count)
	{
		typedef g_impl local;
		return local::f(x) ? count + x : count;
	};
};

ff::IO<void> hnMain()
{
	typedef hnMain_impl local;
	return ff::print(ff::natrec<int>(&local::g, 0, 999));
};
