#include <hn/lib.hpp>

template <typename t3>
std::list<t3> g(std::list<t3> l, boost::function<bool (t3)> f)
{
	return ff::filter(&f, l);
};
