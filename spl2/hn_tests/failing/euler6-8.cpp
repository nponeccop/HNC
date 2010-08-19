#include <hn/lib.hpp>

struct natr_impl
{
	boost::function<int (int)> f;

	int g(int x, int total)
	{
		return total + f(x);
	};
};

int natr(boost::function<int (int)> f)
{
	typedef natr_impl local;
	local impl = { f };
	return ff::natrec<int>(hn::bind(impl, &local::g), 0, 100);
};
