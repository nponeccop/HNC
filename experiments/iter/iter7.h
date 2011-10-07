template <typename T>
struct OurList
{
	T value;
	OurList<T> *next;
};

inline int foldList(OurList<int> *l)
{
	int c = 2;
	int acc = 4;
	for (; l != NULL; l = l->next;)
	{
		acc = l->value * acc + c;
	}
	return acc;
}