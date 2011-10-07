#include <numeric>
#include <list>

class F
{
	int c;
public:
	int operator()(int x, int y)
	{
		return x * y + c;
	}
	F(int _c) : c(_c) {};
};

int foldList(std::list<int> l)
{
	F f(2);
	return std::accumulate(l.begin(), l.end(), 4, f);
}
