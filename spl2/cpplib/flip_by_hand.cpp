#include <hn/lib.hpp>
#include <boost/functional.hpp>

template <typename F>
struct flip_impl
{
	typedef deduce<F> dF;

	F f;

	typename dF::result_type operator()(typename dF::arg<1>::type b, typename dF::arg<0>::type a)
	{
		return f(a, b);
	}

	flip_impl(F _f) : f(_f)
	{
			
	}	
};

template <typename F>
flip_impl<F> flip(F f)
{
	return f;
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