#include <iostream>

template <typename RET, typename A0>
struct polyfunc
{
	typedef RET (type)(A0);
};

template< typename T > 
int size (T)
{
	return sizeof( T );
}

template <typename T>
int g (typename polyfunc<int, T>::type f, T t)
{
	return f(t);
}

int main() {
	std::cout << g( &size, 49090 ) << std::endl;
	return 0;
}

