#include <hn/lib.hpp>

template <typename t2>
bool loopCond(std::pair<t2, ff::ptr<int>> loopState)
{
	ff::ptr<int> s = ff::snd(loopState);
	return ff::deref(s) == 0;
};
