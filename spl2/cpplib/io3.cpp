#include <hn/lib.hpp>

ff::IO<void> hnMain()
{
	return ff::bind(ff::readnum, &ff::print<int>);
};
