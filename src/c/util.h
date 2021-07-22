#ifndef COMMON_H_INCLUDED
#define COMMON_H_INCLUDED

#ifdef BIG
	#define MAX_TIME 30.0
#elif SMALL
	#define MAX_TIME 1.0
#else
	#error No dataset size passed during compilation, BIG or SMALL must be defined.
#endif

#define SNAPSHOT_INTERVAL 50


/* Borrowed from util-linux-2.13-pre7/schedutils/taskset.c */
static char *cpuset_to_cstr(cpu_set_t *mask, char *str)
{
  char *ptr = str;
  int i, j, entry_made = 0;
  for (i = 0; i < CPU_SETSIZE; i++) {
    if (CPU_ISSET(i, mask)) {
      int run = 0;
      entry_made = 1;
      for (j = i + 1; j < CPU_SETSIZE; j++) {
        if (CPU_ISSET(j, mask)) run++;
        else break;
      }
      if (!run)
        sprintf(ptr, "%d,", i);
      else if (run == 1) {
        sprintf(ptr, "%d,%d,", i, i + 1);
        i++;
      } else {
        sprintf(ptr, "%d-%d,", i, i + run);
        i += run;
      }
      while (*ptr != 0) ptr++;
    }
  }
  ptr -= entry_made;
  *ptr = 0;
  return(str);
}

void report_placement()
{
  int rank, thread;
  cpu_set_t coremask;
  char clbuf[7 * CPU_SETSIZE], hnbuf[64];

  MPI_Comm_rank(MPI_COMM_WORLD, &rank);
  memset(clbuf, 0, sizeof(clbuf));
  memset(hnbuf, 0, sizeof(hnbuf));
  (void)gethostname(hnbuf, sizeof(hnbuf));

  int size;
  MPI_Comm_size(MPI_COMM_WORLD, &size);

  if(rank == 0)
  {
	  printf("+-----+--------+---------------+-------------------------------------------------+\n");
	  printf("| MPI | OPENMP | CORE AFFINITY | NODE                                            |\n");
	  printf("+-----+--------+---------------+-------------------------------------------------+\n");
  }
  for(int j = 0; j < size; j++)
  {
	  if(j == rank)
	  {
		  #pragma omp parallel private(thread, coremask, clbuf)
		  {
			thread = omp_get_thread_num();
			(void)sched_getaffinity(0, sizeof(coremask), &coremask);
			cpuset_to_cstr(&coremask, clbuf);
			for(int i = 0; i < omp_get_num_threads(); i++)
			{
				if(i == thread)
				{
					printf("| %3d | %6d | %13s | %47s |\n",  rank, thread, clbuf, hnbuf);
				}
			}
		  }
	  }
  }
  printf("+-----+--------+---------------+-------------------------------------------------+\n");
}

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
