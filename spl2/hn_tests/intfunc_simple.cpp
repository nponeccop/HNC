#include <hn/lib.hpp>

struct hnMain_impl
{
	template <typename t3>
	struct intfunc_impl
	{
		boost::function<t3 (int)> f;

		t3 g(int x)
		{
			return f(x + 0);
		};
	};

	template <typename t3>
	static boost::function<t3 (int)> intfunc(boost::function<t3 (int)> f)
	{
		typedef intfunc_impl<t3> local;
		local impl = { f };
		return hn::bind(impl, &local::g);
	};
};

ff::IO<void> hnMain()
{
	typedef hnMain_impl local;
	return (local::intfunc<ff::IO<void>>(&ff::print<int>))(5);
};
