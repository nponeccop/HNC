#include <winsock.h>

namespace ff
{
	int incr(int);
	int sum(int, int);
	bool less(int, int);

	template <typename T> struct elist
	{
		static std::list<T> value;
	};

	template <typename T> std::list<T> elist<T>::value = std::list<T>();

	template <typename T>
	struct thunk
	{
		typedef boost::function<T ()> type;
	};

	template <typename T> struct IO
	{
		typedef T value_type;

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
	typename IO<T2> bind(IO<T1> a1, boost::function<IO<T2> (T1)> a2)
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



	extern IO<int> readnum;

	template <typename T>
	T read()
	{
		T t;
		std::cin >> t;
		return t;
	}

	template <typename A>
	A natrec(boost::function <A (int, A)> plus, A unit, int start)
	{
		A res = unit;
		for (int i = 0; i <= start ; i++)
		{
			res = plus(i, res);
		}
		return res;
	}

	inline bool eq(int a, int b)
	{
		return a == b;
	}

	inline int mod(int a, int b)
	{
		return a % b;
	}

	inline bool _or(int a, int b)
	{
		return a || b;
	}

	inline int mul(int a, int b)
	{
		return a * b;
	}

	inline int div(int a, int b)
	{
		return a / b;
	}

	inline int sub(int a, int b)
	{
		return a - b;
	}

	template <typename T>
	T _if(bool cond, T t, T f)
	{
		return cond ? t : f;
	}

	template <typename T>
	std::list<T> filter(boost::function<bool (T)> f, std::list<T> in)
	{
		// TODO implement ff::filter - only signature is there
		return in;
	}

	template <typename T>
	std::list<T> join1(T a, std::list<T> l)
	{
		// TODO implement ff::join1 - only signature is there
		return l;
	}

	struct RaiiSocket
	{
		SOCKET s;

		RaiiSocket();
		~RaiiSocket();
	};

	struct UdpSocket : public RaiiSocket
	{
		sockaddr_in lastSender;

		UdpSocket(int);
		UdpSocket(std::string, int);
		~UdpSocket();
		std::string Receive();
		void Send(std::string);
		void Reply(const std::string &);

		void printthis()
		{
			return;
			printf("this = %p\n", this);
		}
		UdpSocket(const UdpSocket &ss)
		{
			puts("UdpSocket::$$CopyConstructor");
			s = ss.s;
		};
	};

	UdpSocket udp_connect(std::string, int);
	UdpSocket udp_listen(int);

	IO<std::string> udp_receive(UdpSocket &);
	IO<void> udp_send(UdpSocket &, std::string);
	IO<void> udp_reply(UdpSocket &, std::string);

	extern IO<int> time_msec;

	IO<void> forever(IO<void>);
};

ff::IO<void> hnMain();
