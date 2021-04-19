


int dummyLoop()
//@ requires true;
//@ ensures result == 3;
{
	int i = 0;
	
	// iteration 1
	if(i<3)
	{
		i = i + 1;
		
		// iteration 2
		if(i<3)
		{
			i = i + 1;
			
				// iteration 3
				if(i<3)
				{
					i = i + 1;
					
						// iteration 4
						if(i<3)
						{
							i = i + 1;
							
							// iteration 5
							if(i<3)
							{
								i = i + 1;
							}
						}
				}
		}
	}
	
	return i;
}