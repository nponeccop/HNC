#pragma once

#include <boost/bind.hpp>
#include <boost/function.hpp>

namespace hn
{
	template< typename ReturnType, typename Target >
	boost::function< ReturnType() > bind( Target a_target, ReturnType (Target::*a_method)() )
	{
		return boost::bind( a_method, a_target );
	}

	template< typename ReturnType, typename Target, typename T0 >
	boost::function< ReturnType( T0 ) > bind( Target a_target, ReturnType (Target::*a_method)( T0 ) )
	{
		return boost::bind( a_method, a_target, _1 );
	}

	template< typename ReturnType, typename Target, typename T0, typename T1 >
	boost::function< ReturnType( T0, T1 ) > bind( Target a_target, ReturnType (Target::*a_method)( T0, T1 ) )
	{
		return boost::bind( a_method, a_target, _1, _2 );
	}

	template< typename ReturnType, typename Target, typename T0, typename T1, typename T2 >
	boost::function< ReturnType( T0, T1, T2 ) > bind( Target a_target, ReturnType (Target::*a_method)( T0, T1, T2 ) )
	{
		return boost::bind( a_method, a_target, _1, _2, _3 );
	}

	template< typename ReturnType, typename Target, typename T0, typename T1, typename T2, typename T3 >
	boost::function< ReturnType( T0, T1, T2, T3 ) > bind( Target a_target, ReturnType (Target::*a_method)( T0, T1, T2, T3 ) )
	{
		return boost::bind( a_method, a_target, _1, _2, _3, _4 );
	}

	template< typename ReturnType, typename Target, typename T0, typename T1, typename T2, typename T3, typename T4 >
	boost::function< ReturnType( T0, T1, T2, T3, T4 ) > bind( Target a_target, ReturnType (Target::*a_method)( T0, T1, T2, T3, T4 ) )
	{
		return boost::bind( a_method, a_target, _1, _2, _3, _4, _5 );
	}
} // namespace hn

