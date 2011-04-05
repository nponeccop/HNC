#include <hn/lib.hpp>

int loopCond(ff::ptr<int> loopState)
{
	return ff::deref(loopState) + 1;
};
