#include <hn/lib.hpp>

struct ii_impl
{
	template <typename t0>
	struct comp_impl
	{
		t0 g;

		template <typename t1>
		t0 h(t1 x)
		{
			return g;
		};
	};

	template <typename t0, typename t2>
	static boost::function<t0 (t2)> comp(t0 g)
	{
		typedef comp_impl<t0> local;
		local impl = { g };
		return hn::bind(impl, &local::h<t2>);
	};
};

boost::function<int (int)> ii()
{
	typedef ii_impl local;
	return local::comp<boost::function<int (int)>, hn::unused>(&ff::incr);
};
