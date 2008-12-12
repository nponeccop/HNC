#include <hn/lib.hpp>

ff::IO<void> hnMain()
{
	//__asm int 3;
	return ff::bind(ff::readnum, ff::fn2(&ff::print<int>));
};
