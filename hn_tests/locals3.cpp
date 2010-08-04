#include <hn/lib.hpp>

struct hnMain_impl
{
	static int main(int x, int z, int l)
	{
		int a = z + 1;
		int y = x + a;
		return y + l;
	};
};

ff::IO<void> hnMain()
{
	typedef hnMain_impl local;
	return ff::print(local::main(2, 4, 7));
};
