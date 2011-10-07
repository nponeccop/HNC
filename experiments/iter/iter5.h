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
	while (l != NULL)
	{
		acc = l->value * acc + c;
		l = l->next;
	}
	return acc;
}

foldList l = {
	c = 2
	acc = 4
	while (l != NULL)
	{
		acc = l->value * acc + c
			l = l->next
	}
	return acc
}


foldList l = {
	c = 2
	acc = 4
	while (not (null l))
	{
		acc = add (mul (head l) acc) c
			l = tail l
	}
	acc
}

foldList l = foldl (\x y -> x * y + c) 4 l where c = 2

foldList l = {
	c = 2
	f x y = add (mul x y) c
	foldl f 4 l
}

foldl a0 a1 a2 = {
	acc = a1
	acc = 4
	while (not (null l))
	{
		acc := a0 (head l) acc
		l := tail l
	}
	acc




}