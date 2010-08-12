#include <hn/lib.hpp>

struct main_impl
{
	template <typename t1, typename t2, typename t5>
	struct flip_impl
	{
		boost::function<boost::function<t5 (t1)> (t2)> f;

		template <typename t1, typename t2, typename t5>
		t5 flipped(t1 x, t2 y)
		{
			return f(y, x);
		};
	};

	template <typename t1, typename t2, typename t5>
	static boost::function<t5 (t1, t2)> flip(boost::function<boost::function<t5 (t1)> (t2)> f)
	{
		typedef flip_impl<t1, t2, t5> local;
		local impl = { f };
		return hn::bind(impl, &local::flipped);
	};
};

t5 main()
{
	typedef main_impl local;
	return (local::flip<int, int, int>(&ff::sum))(3, 2);
};
