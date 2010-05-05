#include <hn/lib.hpp>

ff::IO<void> hnMain()
{
	return ff::print(ff::natrec(&ff::sum, 0, 1));
};
