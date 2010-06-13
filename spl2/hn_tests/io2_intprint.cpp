#include <hn/lib.hpp>

struct hnMain_impl
{
	static ff::IO<void> intprint(int x)
	{
		return ff::print(ff::sum(x, 0));
	};
};

ff::IO<void> hnMain()
{
	typedef hnMain_impl local;
	return ff::bind(ff::readnum, &local::intprint);
};
