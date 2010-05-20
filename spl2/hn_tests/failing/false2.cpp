#include <hn/lib.hpp>

struct hMain_impl
{
	static int f(int x)
	{
		std::string z = "5";
		return ff::sum(x, x);
	};
};

ff::IO<void> hMain()
{
	int z = 7;
	typedef hMain_impl local;
	return ff::print(ff::sum(5, z));
};
