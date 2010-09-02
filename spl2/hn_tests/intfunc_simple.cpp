#include <hn/lib.hpp>

struct hnMain_impl
{
	template <typename t2>
	struct intfunc_impl
	{
		boost::function<t2 (int)> f;

		t2 g(int x)
		{
			return f(x + 0);
		};
	};

	template <typename t2>
	static boost::function<t2 (int)> intfunc(boost::function<t2 (int)> f)
	{
		typedef intfunc_impl<t2> local;
		local impl = { f };
		return hn::bind(impl, &local::g);
	};
};

ff::IO<void> hnMain()
{
	typedef hnMain_impl local;
	return (local::intfunc<ff::IO<void>>(&ff::print<int>))(5);
};
