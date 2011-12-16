#include <hn/lib.hpp>

template <typename t5>
int loopCond(std::pair<t5, ff::ptr<int>> loopState)
{
	return ff::deref(ff::snd(loopState)) + 1;
};
