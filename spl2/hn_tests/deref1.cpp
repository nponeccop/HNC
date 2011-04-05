#include <hn/lib.hpp>

bool f(ff::ptr<int> s)
{
	return ff::deref(s) == 0;
};
