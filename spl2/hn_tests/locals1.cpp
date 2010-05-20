#include <hn/lib.hpp>

struct hnMain_impl
{
	struct main_impl
	{
		int x;
		int a;

		bool y(int z)
		{
			return ff::less(ff::sum(x, a), z);
		};
	};

	static std::list<int> main(int x, int z, std::list<int> l)
	{
		typedef main_impl local;
		local impl = { x, ff::incr(z) };
		return ff::filter(&hn::bind(impl, &local::y), l);
	};
};

ff::IO<void> hnMain()
{
	typedef hnMain_impl local;
	return ff::print("foo");
};
