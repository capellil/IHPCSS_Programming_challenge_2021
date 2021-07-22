#ifndef COMMON_H_INCLUDED
#define COMMON_H_INCLUDED

#ifdef BIG
	#define MAX_TIME 30.0
#elif SMALL
	#define MAX_TIME 5.0
#else
	#error No dataset size passed during compilation, BIG or SMALL must be defined.
#endif

#define SNAPSHOT_INTERVAL 25

void initialise_temperatures(double temperatures[ROWS][COLUMNS])
{
	#ifdef BIG
		int MID_ROWS = ROWS/2;
		int MID_COLUMNS = COLUMNS/2;
		int THICKNESS = ROWS / 2;
		for(int i = 0; i < ROWS; i++)
		{
			for(int j = 0; j < COLUMNS; j++)
			{
				if(i >= (MID_ROWS - THICKNESS/2) && i <= (MID_ROWS + THICKNESS/2) &&
				   j >= (MID_COLUMNS - THICKNESS/2) && j <= (MID_COLUMNS + THICKNESS/2))
				{
					temperatures[i][j] = MAX_TEMPERATURE;
				}
				else
				{
					temperatures[i][j] = 0.0;
				}
			}
		}
	#else
		for(int i = 0; i < ROWS; i++)
		{
			for(int j = 0; j < COLUMNS; j++)
			{
				temperatures[i][j] = (j % 100 == 0) ? MAX_TEMPERATURE : 0.0;
			}
		}
	#endif
}

#endif
