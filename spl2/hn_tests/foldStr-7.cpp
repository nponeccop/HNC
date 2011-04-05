#include <hn/lib.hpp>

template <typename t4>
int loopCond(std::pair<t4, ff::ptr<int>> loopState)
{
	return ff::deref(ff::snd(loopState)) + 1;
};
