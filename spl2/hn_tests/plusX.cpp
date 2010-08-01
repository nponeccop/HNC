#include <hn/lib.hpp>

struct plusX_impl
{
	template <typename t1>
	static t1 f(t1 y)
	{
		return y;
	};
};

template <typename t0, typename t2>
boost::function<t2 (t2)> plusX(t0 x)
{
	typedef plusX_impl local;
	return &local::f<t2, t2>;
};
