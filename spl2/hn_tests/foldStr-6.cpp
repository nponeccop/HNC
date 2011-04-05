#include <hn/lib.hpp>

template <typename t1>
bool loopCond(std::pair<t1, ff::ptr<int>> loopState)
{
	ff::ptr<int> s = ff::snd(loopState);
	return ff::deref(s) == 0;
};
