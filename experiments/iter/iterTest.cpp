#include <iostream>
#include <list>

int foldList(std::list<int> l);

int main(int, char **)
{
	std::list<int> l;
	l.push_front(3);
	std::cout << foldList(l) << std::endl;
}
