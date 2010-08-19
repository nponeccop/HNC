#include <hn/lib.hpp>

struct hnMain_impl
{
	static int natr(boost::function<int (int, int)> f)
	{
		return ff::natrec<int>(f, 6, 5);
	};
	template <typename t6, typename t7>
	static int id(t6 x, t7 y)
	{
		return 5;
	};
};

ff::IO<void> hnMain()
{
	typedef hnMain_impl local;
	return ff::print(local::natr(&local::id<int, int>));
};
