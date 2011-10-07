#include <hn/lib.hpp>

struct hnMain_impl
{
	static ff::IO<void> intprint(int x)
	{
		return ff::print(x + 0);
	};
};

ff::IO<void> hnMain()
{
	typedef hnMain_impl local;
	return ff::bind<int, void>(ff::readnum, &local::intprint);
};
