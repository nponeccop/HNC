#include <hn/lib.hpp>

struct allDigits_impl
{
	static bool loopCond(ff::ptr<int> s)
	{
		return ff::deref(s) == 0;
	};
};

bool allDigits(ff::ptr<int> ss)
{
	typedef allDigits_impl local;
	ff::ptr<int> firstFailure = ss;
	while (local::loopCond(firstFailure))
	{
		firstFailure = ff::next(firstFailure);
	}
	return ff::deref(firstFailure) == 0;
};
