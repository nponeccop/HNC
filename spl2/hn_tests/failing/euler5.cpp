#include <hn/lib.hpp>

struct hnMain_impl
{
	struct f_impl
	{
		struct not_divisable_impl
		{
			int x;

			struct natfind_impl
			{
				boost::function<bool (int)> pred;

				template <typename t5>
				bool ff(t5 nn, bool found)
				{
					return found ? found : pred(nn);
				};
			};

			static bool natfind(boost::function<bool (int)> pred, int n)
			{
				typedef natfind_impl local;
				local impl = { pred };
				return ff::natrec<bool>(hn::bind(impl, &local::ff), 0 == 1, n);
			};
			bool g(int divisor)
			{
				return 0 == x % divisor + 1 ? 0 == 1 : 0 == 0;
			};
		};

		static bool not_divisable(int x)
		{
			typedef not_divisable_impl local;
			local impl = { x };
			return local::natfind(hn::bind(impl, &local::g), 20);
		};
	};

	static int f(int xx, int rr)
	{
		int xxx = 60 * 19 * xx + 1;
		typedef f_impl local;
		return rr == 0 ? local::not_divisable(xxx) ? rr : xxx : rr;
	};
};

ff::IO<void> hnMain()
{
	typedef hnMain_impl local;
	return ff::print(ff::natrec<int>(&local::f, 0, 1000000));
};
