#include <hn/lib.hpp>

struct hnMain_impl
{
	struct f_impl
	{
		struct not_divisable_impl
		{
			int x;

			template <typename t5>
			struct natfind_impl
			{
				boost::function<bool (t5)> pred;

				template <typename t5>
				bool ff(t5 nn, bool found)
				{
					return found ? found : pred(nn);
				};
			};

			template <typename t5>
			static bool natfind(boost::function<bool (t5)> pred, int n)
			{
				typedef natfind_impl<t5> local;
				local impl = { pred };
				return ff::natrec(hn::bind(impl, &local::ff), 0 == 1, n);
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

	template <typename t70>
	static t70 f(int xx, int rr)
	{
		int xxx = 60 * 19 * xx + 1;
		typedef f_impl local;
		return rr == 0 ? local::not_divisable(xxx) ? rr : xxx : rr;
	};
};

ff::IO<void> hnMain()
{
	typedef hnMain_impl local;
	return ff::print(ff::natrec(&local::f<int, int>, 0, 1000000));
};
