#include <hn/lib.hpp>

template <typename _f0, typename x0, typename y0>
struct flip_impl
{
	boost::function<_f0 (y0, x0)> f;

	_f0 flipped(x0 x, y0 y)
	{
		return f(y, x);
	};
};

template <typename _f0, typename x0, typename y0>
boost::function<_f0 (x0, y0)> flip(boost::function<_f0 (y0, x0)> f)
{
	typedef flip_impl<_f0, x0, y0> local;
	local impl = { f };
	return hn::bind(impl, &local::flipped);
};
