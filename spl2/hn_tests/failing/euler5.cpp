#include <hn/lib.hpp>

template <typename t70>
struct hnMain_impl
{
	struct f_impl
	{
		template <typename t5>
		struct not_divisable_impl
		{
			int x;

			template <typename t5>
			struct natfind_impl
			{
				boost::function<bool (t5)> pred;

				bool ff(t5 nn, bool found)
				{
					return ff::_if(found, found, pred(nn));
				};
			};

			static bool natfind(boost::function<bool (t5)> pred, int n)
			{
				typedef natfind_impl<t5> local;
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
			typedef not_divisable_impl<t5> local;
			local impl = { x };
			return local::natfind(hn::bind(impl, &local::g), 20);
		};
	};

	static t70 f(int xx, int rr)
	{
		int xxx = ff::mul(60, ff::mul(19, ff::incr(xx)));
		typedef f_impl local;
		return ff::_if(ff::eq(rr, 0), ff::_if(local::not_divisable(xxx), rr, xxx), rr);
	};
};

ff::IO<void> hnMain()
{
	typedef hnMain_impl<t70> local;
	return ff::print(ff::natrec(&local::f<int, int>, 0, 1000000));
};
