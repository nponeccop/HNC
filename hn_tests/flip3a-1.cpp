#include <hn/lib.hpp>

struct main_impl
{
	template <typename t1, typename t2>
	struct flip_impl
	{
		boost::function<t2 (t1)> f;

		t2 flipped(t1 x)
		{
			return f(x);
		};
	};

	template <typename t1, typename t2>
	static boost::function<t2 (t1)> flip(boost::function<t2 (t1)> f)
	{
		typedef flip_impl<t1, t2> local;
		local impl = { f };
		return hn::bind(impl, &local::flipped);
	};
};

int main()
{
	typedef main_impl local;
	return (local::flip<int, int>(&ff::incr))(3);
};
