#include <numeric>
#include <list>

int foldList(std::list<int> l)
{
	struct 
	{
		int c;
		int operator()(int x, int y)
		{
			return x * y + c;
		}
	} f = { 2 };
	return std::accumulate(l.begin(), l.end(), 4, f);
}
