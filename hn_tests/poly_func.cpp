#include <hn/lib.hpp>

template <typename t2>
std::list<t2> g(std::list<t2> l, boost::function<bool (t2)> f)
{
	return ff::filter<t2>(f, l);
};
