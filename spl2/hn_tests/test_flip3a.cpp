#include <hn/lib.hpp>

template <typename t1, typename t2, typename t5, typename t6, typename t7, typename t8>
struct main_impl
{
	template <typename t1, typename t2, typename t5>
	struct flip_impl
	{
		boost::function<boost::function<t5 (t1)> (t2)> f;

		t5 flipped(t1 x, t2 y)
		{
			return f(y, x);
		};
	};

	static boost::function<t8 (t6, t7)> flip(boost::function<boost::function<t5 (t1)> (t2)> f)
	{
		typedef flip_impl<t1, t2, t5> local;
		local impl = { f };
		return &hn::bind(impl, &local::flipped)<t6, t7, t8>;
	};
};

template <typename t17>
t17 main()
{
	typedef main_impl<t1, t2, t5, t6, t7, t8> local;
	return (main_impl<t1, t2, t5, t6, t7, t8>::flip(&ff::sum))(3, 2);
};
