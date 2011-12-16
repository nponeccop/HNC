#include <hn/lib.hpp>

struct hoand_impl
{
	template <typename t4, typename t5, typename t6>
	struct comp_impl
	{
		boost::function<t5 (t6)> f;
		boost::function<t6 (t4)> g;

		t5 h(t4 x)
		{
			return f(g(x));
		};
	};

	template <typename t4, typename t5, typename t6>
	static boost::function<t5 (t4)> comp(boost::function<t5 (t6)> f, boost::function<t6 (t4)> g)
	{
		typedef comp_impl<t4, t5, t6> local;
		local impl = { f, g };
		return hn::bind(impl, &local::h);
	};
	template <typename t10, typename t8, typename t9>
	struct papp_impl
	{
		boost::function<t10 (t8, t9)> f;
		t8 x;

		t10 g(t9 y)
		{
			return f(x, y);
		};
	};

	template <typename t10, typename t8, typename t9>
	static boost::function<t10 (t9)> papp(boost::function<t10 (t8, t9)> f, t8 x)
	{
		typedef papp_impl<t10, t8, t9> local;
		local impl = { f, x };
		return hn::bind(impl, &local::g);
	};
	template <typename t12, typename t13, typename t14>
	struct flip_impl
	{
		boost::function<t14 (t13, t12)> f;

		t14 g(t12 x, t13 y)
		{
			return f(y, x);
		};
	};

	template <typename t12, typename t13, typename t14>
	static boost::function<t14 (t12, t13)> flip(boost::function<t14 (t13, t12)> f)
	{
		typedef flip_impl<t12, t13, t14> local;
		local impl = { f };
		return hn::bind(impl, &local::g);
	};
};

bool hoand(bool a, bool b)
{
	typedef hoand_impl local;
	boost::function<boost::function<bool (bool)> (bool)> g = local::comp<bool, boost::function<bool (bool)>, bool>(local::papp<boost::function<bool (bool)>, boost::function<bool (bool, bool)>, bool>(&local::papp<bool, bool, bool>, &ff::_or), &ff::not);
	boost::function<boost::function<bool (bool)> (bool)> f = local::comp<bool, boost::function<bool (bool)>, boost::function<bool (bool)>>(local::papp<boost::function<bool (bool)>, boost::function<bool (bool)>, boost::function<bool (bool)>>(local::flip<boost::function<bool (bool)>, boost::function<bool (bool)>, boost::function<bool (bool)>>(&local::comp<bool, bool, bool>), &ff::not), g);
	return (f(a))(b);
};
