#include <hn/lib.hpp>

template <typename t3, typename t7>
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

	static boost::function<t7 (int)> intfunc(boost::function<t3 (int)> f)
	{
		typedef intfunc_impl<t3> local;
		local impl = { f };
		return hn::bind(impl, &local::g);
	};
};

template <typename t10>
ff::IO<t10> hnMain()
{
	typedef hnMain_impl<t3, t7> local;
	return ff::bind(ff::readnum, local::intfunc(&ff::print<t17>));
};
