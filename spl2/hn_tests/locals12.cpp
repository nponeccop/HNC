#include <hn/lib.hpp>

struct hnMain_impl
{
	template <typename t0>
	static int main(t0 x, int z)
	{
		int a = z + 1;
		int y = a;
		return y;
	};
};

ff::IO<void> hnMain()
{
	typedef hnMain_impl local;
	return ff::print(local::main(2, 4));
};
