#include <hn/lib.hpp>

template <typename _f0, typename a, typename b, typename c, typename x0, typename y0>
struct flip_impl
{
	boost::function<c (a, b)> f;

	_f0 flipped(x0 x, y0 y)
	{
		return f(y, x);
	};
};

template <typename a, typename b, typename c>
boost::function<c (b, a)> flip(boost::function<c (a, b)> f)
{
	typedef flip_impl<_f0, a, b, c, x0, y0> local;
	local impl = { f };
	return hn::bind(impl, &local::flipped);
};
