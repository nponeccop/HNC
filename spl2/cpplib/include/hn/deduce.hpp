#pragma once

#include <boost/function_types/result_type.hpp>
#include <boost/function_types/parameter_types.hpp>
#include <boost/function_types/function_arity.hpp>

#include <boost/function.hpp>


template <typename F, bool isBuiltin = boost::function_types::is_callable_builtin<F>::value>
struct deduce
{

};


template <typename F>
struct deduce<F, false>
{
	typedef typename F::result_type result_type;

	static const long arity = F::arity;

	template <long N>
	struct arg
	{
	};
	
	template <>
	struct arg<0>
	{
	       typedef typename F::arg1_type type;
	};

};


template <typename F>
struct deduce<F, true>
{
	typedef typename boost::function_types::result_type<F>::type result_type;

	static const long arity = boost::function_types::function_arity<F>::value;

	template <long N>
	struct arg
	{
		typedef typename boost::mpl::at_c<typename boost::function_types::parameter_types<F>, N>::type type;
	};

};
