#include <hn/lib.hpp>

ff::IO<void> hnMain()
{
	int x = 2;
	int y = 3;
	return ff::print(ff::sum(x, y));
};
