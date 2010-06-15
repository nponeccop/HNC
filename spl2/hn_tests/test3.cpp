#include <hn/lib.hpp>

struct hnMain_impl
{
	struct plusX_impl
	{
		int x;

		int f(int y)
		{
			return x + y;
		};
	};

	static boost::function<int (int)> plusX(int x)
	{
		typedef plusX_impl local;
		local impl = { x };
		return hn::bind(impl, &local::f);
	};
};

ff::IO<void> hnMain()
{
	typedef hnMain_impl local;
	return ff::print((local::plusX(5))(4));
};
