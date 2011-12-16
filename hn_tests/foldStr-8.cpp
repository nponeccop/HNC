#include <hn/lib.hpp>

template <typename t1>
t1 loopCond(ff::ptr<t1> loopState)
{
	return ff::deref(loopState);
};
