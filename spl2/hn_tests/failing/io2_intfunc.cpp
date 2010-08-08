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

	template <typename t3>
	static boost::function<t3 (int)> intfunc(boost::function<t3 (int)> f)
	{
		typedef intfunc_impl<t3> local;
		local impl = { f };
		return hn::bind(impl, &local::g);
	};
};

template <typename t9>
ff::IO<t9> hnMain()
{
	typedef hnMain_impl local;
	return ff::bind(ff::readnum, local::intfunc(&ff::print<t15>));
};
