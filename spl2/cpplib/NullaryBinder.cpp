#pragma once

#include "hn/lib.hpp"

struct f_impl
{
	const char *x;

	void bar()
	{
		puts(x);
	}
};


template <typename F>
void apply0(F f)
{
	f();
}


void main()
{
	f_impl impl = { "foo" };
	typedef f_impl local;
	apply0(hn::bind(impl, &local::bar));
}
