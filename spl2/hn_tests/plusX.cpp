#include <hn/lib.hpp>

template <typename t1>
struct plusX_impl
{
	static t1 f(t1 y)
	{
		return y;
	};
};

template <typename t0, typename t2>
boost::function<t2 (t2)> plusX(t0 x)
{
	typedef plusX_impl<t1> local;
	return &plusX_impl<t1>::f;
};
