#include <numeric>
#include <list>

struct F
{
	int c;
	int operator()(int x, int y)
	{
		return x * y + c;
	}
};

int foldList(std::list<int> l)
{
	F f = { 2 };
	return std::accumulate(l.begin(), l.end(), 4, f);
}
