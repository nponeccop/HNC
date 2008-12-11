#include "hn/lib.hpp"

namespace ff {

int incr(int i)
{
	return i + 1;
}

int sum(int x, int y)
{
	return x + y;
}

IO<int> readnum = IO<int>(&read<int>);

};

int main()
{
	hnMain();
	return 0;
}
