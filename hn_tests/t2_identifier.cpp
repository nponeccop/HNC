#include <hn/lib.hpp>

struct hnMain_impl
{
	template <typename t0>
	static ff::IO<void> t2(t0 x)
	{
		return ff::print(x);
	};
};

ff::IO<void> hnMain()
{
	typedef hnMain_impl local;
	return ff::bind<int, void>(ff::readnum, &local::t2<int>);
};
