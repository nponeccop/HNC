#include <hn/lib.hpp>

template <typename t1, typename t2, typename t3>
struct flip_impl
{
	boost::function<t3 (t2, t1)> f;

	t3 flipped(t1 x, t2 y)
	{
		return f(y, x);
	};
};

template <typename t1, typename t2, typename t3>
boost::function<t3 (t1, t2)> flip(boost::function<t3 (t2, t1)> f)
{
	typedef flip_impl<t1, t2, t3> local;
	local impl = { f };
	return hn::bind(impl, &local::flipped);
};
