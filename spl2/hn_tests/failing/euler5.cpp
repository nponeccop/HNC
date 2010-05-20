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

				bool ff(int nn, bool found)
				{
					return ff::_if(found, found, pred(nn));
				};
			};

			static bool natfind(boost::function<bool (int)> pred, int n)
			{
				typedef natfind_impl local;
				local impl = { pred };
				return ff::natrec(hn::bind(impl, &local::ff), ff::eq(0, 1), n);
			};
			bool g(int divisor)
			{
				return ff::_if(ff::eq(0, ff::mod(x, ff::incr(divisor))), ff::eq(0, 1), ff::eq(0, 0));
			};
		};

		static bool not_divisable(int x)
		{
			typedef not_divisable_impl local;
			local impl = { x };
			return not_divisable_impl::natfind(hn::bind(impl, &local::g), 20);
		};
	};

	static int f(int xx, int rr)
	{
		int xxx = ff::mul(60, ff::mul(19, ff::incr(xx)));
		typedef f_impl local;
		return ff::_if(ff::eq(rr, 0), ff::_if(f_impl::not_divisable(xxx), rr, xxx), rr);
	};
};

ff::IO<void> hnMain()
{
	typedef hnMain_impl local;
	return ff::print(ff::natrec(&hnMain_impl::f, 0, 1000000));
};
