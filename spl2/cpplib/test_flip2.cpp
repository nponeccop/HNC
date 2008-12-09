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


boost::function<int (int, int)> fn2(int (*f)(int, int))
{
	return boost::function<int (int, int)>(f);
}

template <typename T> struct toBF
{
};

template <> struct toBF<int (*)(int, int)>
{
	typedef boost::function<int (int, int)> type;
};

template <typename T>
typename toBF<T>::type fn(T t)
{
	return toBF<T>::type(t);
}
void main()
{
	boost::function<int (int, int)> _sub = sub;
	ff::print(flip(_sub)(3, 2));
	ff::print(flip(fn(sub))(3, 2));
	ff::print(flip(fn2(sub))(3, 2));
	ff::print(flip<int, int, int>(sub)(3, 2));
}
