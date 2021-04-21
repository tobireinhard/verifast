int dummyLoop()
//@ requires true;
//@ ensures result == 3;
//@ unroll 13;
{
	int i = 0;
	while(i<3)
//	//@ invariant 0 <= i &*& i <= 3;
//	//@ invariant true;
	//@ invariant 0 <= i &*& i <= 3;
	{
		i = i + 1;
	}

	return i;
}
