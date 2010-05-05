#include <hn/lib.hpp>

ff::IO<void> hnMain()
{
	int x = 2;
	return ff::print(ff::incr(x));
};
