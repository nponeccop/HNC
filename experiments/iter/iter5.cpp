template <typename T>
struct OurList
{
	T value;
	OurList<T> *next;
};

int foldList(OurList<int> *l)
{
	int c = 2;
	int acc = 4;
	while (l != NULL)
	{
		acc = l->value * acc + c;
		l = l->next;
	}
	return acc;
}