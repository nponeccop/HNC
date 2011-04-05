#include <hn/lib.hpp>

template <typename t2>
t2 loopCond(ff::ptr<t2> loopState)
{
	return ff::deref(loopState);
};
