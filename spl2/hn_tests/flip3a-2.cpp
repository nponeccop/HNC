#include <hn/lib.hpp>

struct main_impl
{
	template <typename t1, typename t3>
	static t3 flip(boost::function<t3 (t1)> f, t1 x)
	{
		return f(x);
	};
};

int main()
{
	typedef main_impl local;
	return local::flip<int, int>(&ff::incr, 3);
};
