#include <hn/lib.hpp>

struct hnMain_impl
{
	template <typename t1, typename t4>
	struct flip_impl
	{
		boost::function<t4 (int, t1)> f;

		t4 flipped(t1 x)
		{
			return f(3, x);
		};
	};

	template <typename t1, typename t4>
	static boost::function<t4 (t1)> flip(boost::function<t4 (int, t1)> f)
	{
		typedef flip_impl<t1, t4> local;
		local impl = { f };
		return hn::bind(impl, &local::flipped);
	};
};

ff::IO<void> hnMain()
{
	typedef hnMain_impl local;
	return ff::print((local::flip<int, int>(&ff::sum))(2));
};
