#include <hn/lib.hpp>

template <typename _f0>
struct intfunc_impl
{
	boost::function<_f0 (int)> f;

	_f0 g(int x)
	{
		return f(ff::sum(x, 0));
	};
};

template <typename _f0>
boost::function<_f0 (int)> intfunc(boost::function<_f0 (int)> f)
{
	typedef intfunc_impl<_f0> local;
	local impl = { f };
	return hn::bind(impl, &local::g);
};
