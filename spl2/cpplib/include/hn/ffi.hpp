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

	template <>
	struct IO<void>
	{
	};

	template <typename T>
	IO<void> print(T t)
	{
		std::cout << t << std::endl;
		return IO<void>();
	}

	inline
	boost::function<IO<void> (int)> fn2(IO<void> (*x)(int))
	{
		return x;
	}

	template <typename T1, typename T2> 
	IO<T2> bind(IO<T1> a1, boost::function<IO<T2> (T1)> a2)
	{
		return a2(a1.value());
	}

	inline
	int read()
	{
		int t;
		std::cin >> t;
		return t;
	}
/*
	template <typename T> 
	struct read_impl
	{
		static T read()
		{
//			T t;
//			t << std::cin;
			return 75;
		}
	};

	template <typename T> 
	IO<T> read()
	{
		typedef read_impl<T> local;
		return IO<T>(boost::function<T ()>(&read_impl<T>::read));
	}
*/
	extern IO<int> readnum;

};

ff::IO<void> hnMain();
