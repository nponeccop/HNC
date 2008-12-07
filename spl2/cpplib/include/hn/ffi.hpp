namespace ff
{
	int incr(int);
	int sum(int, int);

	template <typename T> struct elist
	{
		static T value;
	};

	template <typename T> std::list<T> elist<T>::value = std::list<T>();

	template <typename T> struct IO
	{
		T value;
	};

	template <>
	struct IO<void>
	{
	};

	const IO<int> x = { 5 };

	const IO<void> y;

	template <typename T>
	IO<void> print(T t)
	{
		std::cout << t << std::endl;
		return IO<void>();
	}

	template <typename T1, typename T2> 
	IO<T2> bind(IO<T1> a1, boost::function<IO<T2> (T1)> a2)
	{
		return a2(a1.value);
	}
};

ff::IO<void> hnMain();
