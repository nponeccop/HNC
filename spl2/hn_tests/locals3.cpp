#include <hn/lib.hpp>

struct hnMain_impl
{
	static int main(int x, int z, int l)
	{
		int a = ff::incr(z);
		int y = ff::sum(x, a);
		return ff::sum(y, l);
	};
};

ff::IO<void> hnMain()
{
	typedef hnMain_impl local;
	return ff::print(hnMain_impl::main(2, 4, 7));
};
