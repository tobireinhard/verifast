int dummyLoop()
//@ requires true;
//@ ensures result == 3;
//@ unroll 4611686018427387904;
{
	int i = 0;
	while(i<3)
	{
		i = i + 1;
	}

	return i;
}
