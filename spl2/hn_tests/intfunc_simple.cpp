#include <hn/lib.hpp>

struct hnMain_impl
{
	template <typename t3>
	struct intfunc_impl
	{
		boost::function<t3 (int)> f;

		template <typename t3>
		t3 g(int x)
		{
			return f(x + 0);
		};
	};

	template <typename t3, typename t7>
	static boost::function<t7 (int)> intfunc(boost::function<t3 (int)> f)
	{
		typedef intfunc_impl<t3> local;
		local impl = { f };
		return hn::bind(impl, &local::g);
	};
};

template <typename t14>
t14 hnMain()
{
	typedef hnMain_impl local;
	return (local::intfunc(&ff::print<t13>))(5);
};
