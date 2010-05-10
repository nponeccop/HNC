#include <hn/lib.hpp>

template <typename y0>
struct plusX_impl
{
	static y0 f(y0 y)
	{
		return y;
	};
};

template <typename x, typename y0>
boost::function<y0 (y0)> plusX(x x)
{
	typedef plusX_impl<y0> local;
	return &plusX_impl<y0>::f;
};
