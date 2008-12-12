namespace ff
{
	int incr(int);
	int sum(int, int);

	template <typename T> struct elist
	{
		static T value;
	};

	template <typename T> std::list<T> elist<T>::value = std::list<T>();

	template <typename T>
	struct thunk
	{
		typedef boost::function<T ()> type;
	};

	template <typename T> struct IO
	{
		typename thunk<T>::type value;
		IO(typename thunk<T>::type _v) : value (_v)
		{
		};
	};

	template <typename T1, typename T2> 
	struct bind_impl
	{
		IO<T1> a1;
		boost::function<IO<T2> (T1)> a2;
		T2 operator()()
		{
			return a2(a1.value()).value();
		}
	};

	template <typename T1, typename T2> 
	IO<T2> bind(IO<T1> a1, boost::function<IO<T2> (T1)> a2)
	{
		bind_impl<T1, T2> impl = { a1, a2 };
		return impl;
	};

	template <typename T2> 
	struct voidbind_impl
	{
		IO<void> a1;
		IO<T2> a2;
		T2 operator()()
		{
			a1.value();
			return a2.value();
		}
	};

	template <typename T2> 
	IO<T2> voidbind(IO<void> a1, IO<T2> a2)
	{
		voidbind_impl<T2> impl = { a1, a2 };
		return impl;
	};



	template <typename T>
	void print_impl(T t)
	{
		std::cout << t << std::endl;		
	}


	template <typename T>
	IO<void> print(T t)
	{
		return boost::bind(&print_impl<T>, t);
	}

	inline
	boost::function<IO<void> (int)> fn2(IO<void> (*x)(int))
	{
		return x;
	}

	extern IO<int> readnum;

	template <typename T> 
	T read()
	{
		T t;
		std::cin >> t;
		return t;
	}

	template <typename F, typename A>
	A natrec(F plus, A unit, int start)
	{
		A res = unit;
		for (int i = 0; i < start ; i++)
		{
			res = plus(i, res);
		}
		return res;
	}

};

ff::IO<void> hnMain();
