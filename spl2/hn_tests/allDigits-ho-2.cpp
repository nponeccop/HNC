#include <hn/lib.hpp>

struct allDigits_impl
{
	template <typename t3, typename t4, typename t5>
	struct comp_impl
	{
		boost::function<t4 (t5)> f;
		boost::function<t5 (t3)> g;

		t4 h(t3 x)
		{
			return f(g(x));
		};
	};

	template <typename t3, typename t4, typename t5>
	static boost::function<t4 (t3)> comp(boost::function<t4 (t5)> f, boost::function<t5 (t3)> g)
	{
		typedef comp_impl<t3, t4, t5> local;
		local impl = { f, g };
		return hn::bind(impl, &local::h);
	};
};

int allDigits(int ss)
{
	typedef allDigits_impl local;
	boost::function<int (int)> loopCond = local::comp<int, int, int>(&ff::incr, &ff::incr);
	return loopCond(ss);
};
