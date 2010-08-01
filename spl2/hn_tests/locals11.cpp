#include <hn/lib.hpp>

struct hnMain_impl
{
	template <typename t0, typename t2>
	static int main(t0 x, int z, t2 l)
	{
		int a = z + 1;
		int y = a;
		return y;
	};
};

ff::IO<void> hnMain()
{
	typedef hnMain_impl local;
	return ff::print(local::main(2, 4, 7));
};
