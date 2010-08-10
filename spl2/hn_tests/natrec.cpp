#include <hn/lib.hpp>

ff::IO<void> hnMain()
{
	return ff::print(ff::natrec<int>(&ff::sum, 0, 1));
};
