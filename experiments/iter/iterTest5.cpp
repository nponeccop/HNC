#include <iostream>
#include "iter5.h"

int main(int, char **)
{
	OurList<int> l = { 3, NULL };
	std::cout << foldList(l) << std::endl;
}
