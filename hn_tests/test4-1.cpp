#include <hn/lib.hpp>

struct hnMain_impl
{
	template <typename t0>
	static t0 id(t0 x)
	{
		return x;
	};
};

ff::IO<void> hnMain()
{
	typedef hnMain_impl local;
	return ff::print(local::id(4));
};
