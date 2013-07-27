#include <hn/lib.hpp>

struct ii_impl
{
	struct comp_impl
	{
		boost::function<int (int)> g;

		template <typename t2>
		int h(t2 x)
		{
			return g(5) + 1;
		};
	};

	template <typename t0, typename t5>
	static boost::function<int (t5)> comp(t0 f, boost::function<int (int)> g)
	{
		typedef comp_impl local;
		local impl = { g };
		return hn::bind(impl, &local::h<t5>);
	};
};

int ii()
{
	typedef ii_impl local;
	return local::comp<int, hn::unused>(6, &ff::incr);
};
