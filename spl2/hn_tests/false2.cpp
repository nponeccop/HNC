#include <hn/lib.hpp>

struct hMain_impl
{
	struct f_impl
	{
		struct g_impl
		{
			template <typename t2>
			static std::string z(t2 x)
			{
				return "5";
			};
		};

		template <typename t1>
		static int g(t1 x)
		{
			typedef g_impl local;
			return 8;
		};
	};

	static int f(int x)
	{
		std::string z = "5";
		typedef f_impl local;
		return x + x;
	};
};

ff::IO<void> hMain()
{
	int z = 7;
	typedef hMain_impl local;
	return ff::print(5 + z);
};
