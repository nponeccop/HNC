#include <hn/lib.hpp>

struct hoand_impl
{
	typedef hoand_impl self;
	template <typename t4, typename t5, typename t6>
	struct comp1_impl
	{
		boost::function<t5 (t6)> f;
		boost::function<t6 (t4)> g;

		t5 h(t4 x)
		{
			return f(g(x));
		};
	};

	template <typename t4, typename t5, typename t6>
	static boost::function<t5 (t4)> comp1(boost::function<t5 (t6)> f, boost::function<t6 (t4)> g)
	{
		typedef comp1_impl<t4, t5, t6> local;
		local impl = { f, g };
		return hn::bind(impl, &local::h);
	};
	template <typename t10, typename t11, typename t9>
	struct comp2_impl
	{
		boost::function<t10 (t11)> f;
		boost::function<t11 (t9)> g;

		t10 h(t9 x)
		{
			return f(g(x));
		};
	};

	template <typename t10, typename t11, typename t9>
	static boost::function<t10 (t9)> comp2(boost::function<t10 (t11)> f, boost::function<t11 (t9)> g)
	{
		typedef comp2_impl<t10, t11, t9> local;
		local impl = { f, g };
		return hn::bind(impl, &local::h);
	};
	template <typename t14, typename t15, typename t16>
	struct comp3_impl
	{
		boost::function<t15 (t16)> f;
		boost::function<t16 (t14)> g;

		t15 h(t14 x)
		{
			return f(g(x));
		};
	};

	template <typename t14, typename t15, typename t16>
	static boost::function<t15 (t14)> comp3(boost::function<t15 (t16)> f, boost::function<t16 (t14)> g)
	{
		typedef comp3_impl<t14, t15, t16> local;
		local impl = { f, g };
		return hn::bind(impl, &local::h);
	};
	template <typename t18, typename t19, typename t20>
	struct papp1_impl
	{
		boost::function<t20 (t18, t19)> f;
		t18 x;

		t20 g(t19 y)
		{
			return f(x, y);
		};
	};

	template <typename t18, typename t19, typename t20>
	static boost::function<t20 (t19)> papp1(boost::function<t20 (t18, t19)> f, t18 x)
	{
		typedef papp1_impl<t18, t19, t20> local;
		local impl = { f, x };
		return hn::bind(impl, &local::g);
	};
	template <typename t22, typename t23, typename t24>
	struct papp2_impl
	{
		boost::function<t24 (t22, t23)> f;
		t22 x;

		t24 g(t23 y)
		{
			return f(x, y);
		};
	};

	template <typename t22, typename t23, typename t24>
	static boost::function<t24 (t23)> papp2(boost::function<t24 (t22, t23)> f, t22 x)
	{
		typedef papp2_impl<t22, t23, t24> local;
		local impl = { f, x };
		return hn::bind(impl, &local::g);
	};
	template <typename t26, typename t27, typename t28>
	struct papp3_impl
	{
		boost::function<t28 (t26, t27)> f;
		t26 x;

		t28 g(t27 y)
		{
			return f(x, y);
		};
	};

	template <typename t26, typename t27, typename t28>
	static boost::function<t28 (t27)> papp3(boost::function<t28 (t26, t27)> f, t26 x)
	{
		typedef papp3_impl<t26, t27, t28> local;
		local impl = { f, x };
		return hn::bind(impl, &local::g);
	};
	template <typename t32, typename t33, typename t34>
	static boost::function<t33 (t32)> g(boost::function<t34 (t32)> x, boost::function<t33 (t34)> y)
	{
		return comp3<t32, t33, t34>(y, x);
	};
};

bool hoand(bool a, bool b)
{
	typedef hoand_impl local;
	return ((local::comp1<bool, boost::function<bool (bool)>, boost::function<bool (bool)>>(local::papp1<boost::function<bool (bool)>, boost::function<bool (bool)>, boost::function<bool (bool)>>(&local::g<bool, bool, bool>, &ff::_not), local::comp2<boost::function<bool (bool)>, bool, bool>(local::papp2<boost::function<bool (bool, bool)>, bool, boost::function<bool (bool)>>(&local::papp3<bool, bool, bool>, &ff::_or), &ff::_not)))(a))(b);
};
