#include <hn/lib.hpp>

struct hnMain_impl
{
	template <typename t3>
	static int f(std::list<t3> l)
	{
		return 1 + ff::length(l);
	};
};

ff::IO<void> hnMain()
{
	typedef hnMain_impl local;
	return ff::print(local::f(ff::join1(1, ff::elist<int>)) + local::f(ff::join1("aaa", ff::elist<std::string>)));
};
