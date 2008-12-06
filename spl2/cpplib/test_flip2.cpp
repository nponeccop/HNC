#include <hn/lib.hpp>

template <typename a, typename b, typename c>
struct flip_impl
{
	boost::function<c (a, b)> f;

	c flipped(b x, a y)
	{
		return f(y, x);
	};
};

template <typename a, typename b, typename c>
boost::function<c (b, a)> flip(boost::function<c (a, b)> f)
{
	typedef flip_impl<a, b, c> local;
	local impl = { f };
	return hn::bind(impl, &local::flipped);
};


int sub(int a, int b)
{
	return a - b;
}

void main()
{
	boost::function<int (int, int)> _sub = sub;
	printf("%d\n", (flip(_sub))(3, 2));
}
