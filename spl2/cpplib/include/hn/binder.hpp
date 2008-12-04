#pragma once

namespace hn {

template <typename IMPL>
struct binder
{
	IMPL * _this;

	void (IMPL::*method)();

	void operator()()
	{
		(_this->*method)();
	}
};


template <typename IMPL>
binder<IMPL> bind(IMPL &_this, void (IMPL::*method)() )
{
	binder<IMPL> b = { &_this, method };
	return b;
}

};