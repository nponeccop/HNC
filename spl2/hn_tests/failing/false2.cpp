#include <hn/lib.hpp>

struct hMain_impl
{
	template <typename t1>
	struct f_impl
	{
		template <typename t2>
		struct g_impl
		{
			static std::string z(t2 x)
			{
				return "5";
			};
		};

		static int g(t1 x)
		{
			typedef g_impl<t2> local;
			return 8;
		};
	};

	static int f(int x)
	{
		std::string z = "5";
		typedef f_impl<t1> local;
		return ff::sum(x, x);
	};
};

ff::IO<void> hMain()
{
	int z = 7;
	typedef hMain_impl local;
	return ff::print(ff::sum(5, z));
};
