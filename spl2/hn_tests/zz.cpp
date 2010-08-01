#include <hn/lib.hpp>

template <typename t0, typename t2>
t2 f(boost::function<t2 (boost::function<t2 (boost::function<t2 (boost::function<t2 (t0)>)>)>)> z)
{
	return z(&z);
};
