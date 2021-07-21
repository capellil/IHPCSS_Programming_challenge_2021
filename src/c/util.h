#ifndef COMMON_H_INCLUDED
#define COMMON_H_INCLUDED

#ifdef BIG
	#define MAX_TIME 30.0
	#define ROWS 15360
	#define COLUMNS 15360

	#ifdef MPI_ONLY
		// 15360 rows divided by 256 MPI processes = 60 rows. With 2 rows for halo, 1 up and 1 down, which gives 60 + 2 = 62 rows per MPI process.
		#define ROWS_PER_MPI_PROCESS 62 
		#define COLUMNS_PER_MPI_PROCESS 15360
	#elif MPI_OPENMP
		// 15360 rows divided by 2x2 MPI processes = 3840 rows. With 2 rows for halo, 1 up and 1 down, which gives 3840 + 2 = 3842 rows per MPI process.
		#define ROWS_PER_MPI_PROCESS 3840 
		#define COLUMNS_PER_MPI_PROCESS 15360
	#elif MPI_OPENACC
		#define ROWS_PER_MPI_PROCESS 1922
		#define COLUMNS_PER_MPI_PROCESS 15360
	#else
		#error No configuration found, MPI_ONLY, MPI_OPENMP or MPI_OPENACC must be defined.
	#endif
#elif SMALL
	#define MAX_TIME 1.0
	#define ROWS 512
	#define COLUMNS 512

	// 512 rows divided by 4 MPI processes = 128 rows. With 2 rows for halo, 1 up and 1 down, which gives 128 + 2 = 130 rows per MPI process.
	#define ROWS_PER_MPI_PROCESS 130
	#define COLUMNS_PER_MPI_PROCESS 512
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
