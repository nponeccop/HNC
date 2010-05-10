#include <hn/lib.hpp>
#include <boost/functional.hpp>

template <typename F>
struct flip_impl
{
	F f;

	typename hn::result<F>::type flipped(
		typename hn::arg<F, 1>::type b, 
		typename hn::arg<F, 0>::type a)
	{
		return f(a, b);
	}
};

template <typename F>
typename boost::function<
	typename hn::result<F>::type (
		typename hn::arg<F, 1>::type b, 
		typename hn::arg<F, 0>::type a)>
flip(F f)
{
	flip_impl<F> impl = { f };
	return hn::bind(impl, &flip_impl<F>::flipped);
}


int sub(int a, int b)
{
	return a - b;
}


struct sub2
{
	typedef int result_type;
	typedef int arg1_type;
	typedef int arg2_type;

	static const int arity = 2;

	int operator()(int a, int b)
	{
		return a - b;
	}


	int sub(int a, int b)
	{
		return a - b;
	}
	
};


struct sub3
{
	int sub(int a, int b)
	{
		return a - b;
	}
};


void main()
{

	sub2 s2;
	sub3 s3;
	boost::function<int (int,int)> s2a = s2;

	printf("%d\n", flip(&sub)(2, 3));
	printf("%d\n", s2(2, 3));
	printf("%d\n", s2a(2, 3));
	printf("%d\n", flip(s2)(2, 3));
	printf("%d\n", hn::bind(s3, &sub3::sub)(2, 3));
	printf("%d\n", flip(hn::bind(s3, &sub3::sub))(2, 3));
}