template <typename A, typename B> A _const(A a, B b)
{
	return a;
}

struct const_impl
{
	A a;
	template <typename B>
	A const(B b)
	{
		return b;
	} 
	
}

struct foo_impl
{
	boost::function<ff::IO<void> (t0)> x = &ff::print<t0>;	
}


int foo(int x)
{
	typedef hnMain_impl local;
	local impl = { x };
	return impl._const("5") + impl._const(5);                
}

