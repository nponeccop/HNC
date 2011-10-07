#include <numeric>
#include <list>

int foldList(std::list<int> l)
{
	int c = 2;
	int acc = 4;
	for (std::list<int>::iterator i = l.begin(); i != l.end(); ++i)
	{
		acc = *i * acc + c;
	}
	return acc;
}
