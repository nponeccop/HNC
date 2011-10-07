#include <hn/lib.hpp>

struct hnMain_impl
{
	template <typename t0>
	static ff::IO<void> tmp(t0 z)
	{
		return ff::print(z);
	};
};

ff::IO<void> hnMain()
{
	typedef hnMain_impl local;
	return local::tmp(5);
};
