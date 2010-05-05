#include <hn/lib.hpp>

ff::IO<void> hnMain()
{
	return ff::voidbind(ff::print(2), ff::print("Hello world"));
};
