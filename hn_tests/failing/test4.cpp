#include <hn/lib.hpp>

struct hnMain_impl
{
	struct plusX_impl
	{
		template <typename t1>
		static t1 f(t1 y)
		{
			return y;
		};
	};

	template <typename t0, typename t2>
	static boost::function<t2 (t2)> plusX(t0 x)
	{
		typedef plusX_impl local;
		return &local::f<t2>;
	};
};

ff::IO<void> hnMain()
{
	typedef hnMain_impl local;
	return ff::print((local::plusX<int, int>(5))(4));
};
