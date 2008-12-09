#include <hn/lib.hpp>

ff::IO<void> hnMain()
{
	return ff::bind(ff::readnum, ff::fn2(&ff::print<int>));
};
