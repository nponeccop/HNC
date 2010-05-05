#include <hn/lib.hpp>

template <typename a>
std::list<a> g(std::list<a> l, boost::function<bool (a)> f)
{
	return ff::filter(f, l);
};
