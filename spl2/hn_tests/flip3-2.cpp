#include <hn/lib.hpp>

struct hnMain_impl
{
	template <typename t1, typename t3>
	struct flip_impl
	{
		boost::function<t3 (t1)> f;

		t3 flipped(t1 x)
		{
			return f(x);
		};
	};

	template <typename t1, typename t3>
	static boost::function<t3 (t1)> flip(boost::function<t3 (t1)> f)
	{
		typedef flip_impl<t1, t3> local;
		local impl = { f };
		return hn::bind(impl, &local::flipped);
	};
};

ff::IO<void> hnMain()
{
	typedef hnMain_impl local;
	return ff::print((local::flip<int, int>(&ff::incr))(2));
};
