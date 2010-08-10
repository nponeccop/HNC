#include <hn/lib.hpp>

ff::IO<void> hnMain()
{
	return ff::bind<int, void>(ff::readnum, &ff::print<int>);
};
