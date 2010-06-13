#include <hn/lib.hpp>

struct hnMain_impl
{
	static int longMulLo(int low, int mp)
	{
		return ff::mod(ff::mul(low, mp), 100);
	};
	static int longMulHi(int hi, int low, int mp)
	{
		return ff::sum(ff::mul(hi, mp), ff::div(ff::mul(low, mp), 100));
	};
};

ff::IO<void> hnMain()
{
	typedef hnMain_impl local;
	return ff::print(local::longMulHi(55, 33, 10));
};
