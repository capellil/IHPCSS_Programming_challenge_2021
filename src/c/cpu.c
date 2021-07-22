/**
 * @file main.c
 * @brief This file contains the source code of the application to parallelise.
 * @details This application is a classic heat spread simulation.
 * @author Ludovic Capelli
 **/

#include <stdio.h>
#include <stdlib.h>
#include <mpi.h>
#include <omp.h>
#include <inttypes.h>
#include <math.h>
#include <sched.h>
#include <unistd.h>
#include <string.h>

#include "util.h"

/**
 * @argv[0] Name of the program
 * @argv[1] path to the dataset to load
 **/
int main(int argc, char* argv[])
{
	MPI_Init(NULL, NULL);

	/////////////////////////////////////////////////////
	// -- PREPARATION 1: COLLECT USEFUL INFORMATION -- //
	/////////////////////////////////////////////////////
	// Ranks for convenience so that we don't throw raw values all over the code
	const int MASTER_PROCESS_RANK = 0;

	// The rank of the MPI process in charge of this instance
	int my_rank;
	MPI_Comm_rank(MPI_COMM_WORLD, &my_rank);

	// Number of MPI processes in total, commonly called "comm_size" for "communicator size".
	int comm_size;
	MPI_Comm_size(MPI_COMM_WORLD, &comm_size);

	/// Rank of the first MPI process
	const int FIRST_PROCESS_RANK = 0;
	/// Rank of the last MPI process
	const int LAST_PROCESS_RANK = comm_size - 1;

	// Rank of my up neighbour if any
	int up_neighbour_rank = (my_rank == FIRST_PROCESS_RANK) ? MPI_PROC_NULL : my_rank - 1;
	
	// Rank of my down neighbour if any
	int down_neighbour_rank = (my_rank == LAST_PROCESS_RANK) ? MPI_PROC_NULL : my_rank + 1;

	//report_placement();

	////////////////////////////////////////////////////////////////////
	// -- PREPARATION 2: INITIALISE TEMPERATURES ON MASTER PROCESS -- //
	////////////////////////////////////////////////////////////////////

	/// Array that will contain my part chunk. It will include the 2 ghost rows (1 up, 1 down)
	double temperatures[ROWS_PER_MPI_PROCESS+2][COLUMNS_PER_MPI_PROCESS];
	/// Temperatures from the previous iteration, same dimensions as the array above.
	double temperatures_last[ROWS_PER_MPI_PROCESS+2][COLUMNS_PER_MPI_PROCESS];
	/// On master process only: contains all temperatures read from input file.
	double all_temperatures[ROWS][COLUMNS];

        // to handle async communication during the simulation, one from up, the other one from down
        MPI_Request requests[2];

	// The master MPI process will read a chunk from the file, send it to the corresponding MPI process and repeat until all chunks are read.
	if(my_rank == MASTER_PROCESS_RANK)
	{
		initialise_temperatures(all_temperatures);
	}

	MPI_Barrier(MPI_COMM_WORLD);

	///////////////////////////////////////////
	//     ^                                 //
	//    / \                                //
	//   / | \    CODE FROM HERE IS TIMED    //
	//  /  o  \                              //
	// /_______\                             //
	///////////////////////////////////////////
	
	////////////////////////////////////////////////////////
	// -- TASK 1: DISTRIBUTE DATA TO ALL MPI PROCESSES -- //
	////////////////////////////////////////////////////////
	double total_time_so_far = 0.0;
	double start_time = MPI_Wtime();

        if(my_rank == MASTER_PROCESS_RANK)
        {
          for(int i = 0; i < comm_size; i++)
          {
            // Is the i'th chunk meant for me, the master MPI process?
            if(i != my_rank)
            {
              // No, so send the corresponding chunk to that MPI process.
              MPI_Send(&all_temperatures[i * ROWS_PER_MPI_PROCESS][0], ROWS_PER_MPI_PROCESS * COLUMNS_PER_MPI_PROCESS, MPI_DOUBLE, i, 0, MPI_COMM_WORLD);
            }
            else
            {
              // Yes, let's copy it straight for the array in which we read the file into.
              for(int j = 1; j <= ROWS_PER_MPI_PROCESS; j++)
              {
                for(int k = 0; k < COLUMNS_PER_MPI_PROCESS; k++)
                {
                  temperatures_last[j][k] = all_temperatures[j-1][k];
                }
              }
            }
          }
        }
        else
        {
          // Receive my chunk.
          MPI_Irecv(&temperatures_last[1][0], ROWS_PER_MPI_PROCESS * COLUMNS_PER_MPI_PROCESS, MPI_DOUBLE, MASTER_PROCESS_RANK, MPI_ANY_TAG, MPI_COMM_WORLD, &requests[0]);
        }

        if(my_rank != MASTER_PROCESS_RANK)
        {
          MPI_Wait(&requests[0], MPI_STATUS_IGNORE);
        }

        // Copy the temperatures into the current iteration temperature as well
        for(int i = 1; i <= ROWS_PER_MPI_PROCESS; i++)
        {
          for(int j = 0; j < COLUMNS_PER_MPI_PROCESS; j++)
          {
            temperatures[i][j] = temperatures_last[i][j];
          }
        }

        if(my_rank == MASTER_PROCESS_RANK)
        {
          printf("Data acquisition complete.\n");
        }

	
	/////////////////////////////
	// TASK 2: DATA PROCESSING //
	/////////////////////////////
	int iteration_count = 0;
	/// Maximum temperature change observed across all MPI processes
	double global_temperature_change;
	/// Maximum temperature change for us
	double my_temperature_change; 
	/// The last snapshot made
	double snapshot[ROWS][COLUMNS];

	while(total_time_so_far < MAX_TIME)
	{
		my_temperature_change = 0.0;

		// ////////////////////////////////////////
		// -- SUBTASK 1: EXCHANGE GHOST CELLS -- //
		// ////////////////////////////////////////

                /* Receive data from up/down neighbour to fill our ghost cells. If my down_neighbour_rank is MPI_PROC_NULL, this MPI_Recv will do nothing. */
                MPI_Irecv(&temperatures_last[ROWS_PER_MPI_PROCESS+1][0], COLUMNS_PER_MPI_PROCESS, MPI_DOUBLE, down_neighbour_rank, MPI_ANY_TAG, MPI_COMM_WORLD, &requests[0]);
                MPI_Irecv(&temperatures_last[0][0], COLUMNS_PER_MPI_PROCESS, MPI_DOUBLE, up_neighbour_rank, MPI_ANY_TAG, MPI_COMM_WORLD, &requests[1]);

                // Send data to up/down neighbour for its ghost cells. If my up_neighbour_rank is MPI_PROC_NULL, this MPI_Ssend will do nothing.
                MPI_Send(&temperatures[1][0], COLUMNS_PER_MPI_PROCESS, MPI_DOUBLE, up_neighbour_rank, 0, MPI_COMM_WORLD);
                MPI_Send(&temperatures[ROWS_PER_MPI_PROCESS][0], COLUMNS_PER_MPI_PROCESS, MPI_DOUBLE, down_neighbour_rank, 0, MPI_COMM_WORLD);

                MPI_Waitall(2, &requests, MPI_STATUS_IGNORE);                

		/////////////////////////////////////////////
		// -- SUBTASK 2: PROPAGATE TEMPERATURES -- //
		/////////////////////////////////////////////
		for(int i = 1; i <= ROWS_PER_MPI_PROCESS; i++)
		{
			// Process the cell at the first column, which has no left neighbour
			if(temperatures[i][0] != MAX_TEMPERATURE)
			{
				temperatures[i][0] = (temperatures_last[i-1][0] +
									  temperatures_last[i+1][0] +
									  temperatures_last[i  ][1]) / 3.0;
			}
			// Process all cells between the first and last columns excluded, which each has both left and right neighbours
			for(int j = 1; j < COLUMNS_PER_MPI_PROCESS - 1; j++)
			{
				if(temperatures[i][j] != MAX_TEMPERATURE)
				{
					temperatures[i][j] = 0.25 * (temperatures_last[i-1][j  ] +
												 temperatures_last[i+1][j  ] +
												 temperatures_last[i  ][j-1] +
												 temperatures_last[i  ][j+1]);
				}
			}
			// Process the cell at the last column, which has no right neighbour
			if(temperatures[i][COLUMNS_PER_MPI_PROCESS - 1] != MAX_TEMPERATURE)
			{
				temperatures[i][COLUMNS_PER_MPI_PROCESS - 1] = (temperatures_last[i-1][COLUMNS_PER_MPI_PROCESS - 1] +
															    temperatures_last[i+1][COLUMNS_PER_MPI_PROCESS - 1] +
															    temperatures_last[i  ][COLUMNS_PER_MPI_PROCESS - 2]) / 3.0;
			}
		}

		///////////////////////////////////////////////////////
		// -- SUBTASK 3: CALCULATE MAX TEMPERATURE CHANGE -- //
		///////////////////////////////////////////////////////
		my_temperature_change = 0.0;
		for(int i = 1; i <= ROWS_PER_MPI_PROCESS; i++)
		{
			for(int j = 0; j < COLUMNS_PER_MPI_PROCESS; j++)
			{
				my_temperature_change = fmax(fabs(temperatures[i][j] - temperatures_last[i][j]), my_temperature_change);
			}
		}
	
		//////////////////////////////////////////////////////////
		// -- SUBTASK 4: FIND MAX TEMPERATURE CHANGE OVERALL -- //
		//////////////////////////////////////////////////////////
                MPI_Allreduce(&my_temperature_change, &global_temperature_change, 1, MPI_DOUBLE,  MPI_MAX, MPI_COMM_WORLD);

                
		//////////////////////////////////////////////////
		// -- SUBTASK 5: UPDATE LAST ITERATION ARRAY -- //
		//////////////////////////////////////////////////
		for(int i = 1; i <= ROWS_PER_MPI_PROCESS; i++)
		{
			for(int j = 0; j < COLUMNS_PER_MPI_PROCESS; j++)
			{
				temperatures_last[i][j] = temperatures[i][j];
			}
		}

		///////////////////////////////////
		// -- SUBTASK 6: GET SNAPSHOT -- //
		///////////////////////////////////
		if(iteration_count % SNAPSHOT_INTERVAL == 0)
		{
                  MPI_Gather(&temperatures[1][0], ROWS_PER_MPI_PROCESS * COLUMNS_PER_MPI_PROCESS, MPI_DOUBLE, &snapshot,
                             ROWS_PER_MPI_PROCESS * COLUMNS_PER_MPI_PROCESS, MPI_DOUBLE,  MASTER_PROCESS_RANK, MPI_COMM_WORLD);

                  if(my_rank == MASTER_PROCESS_RANK)
                  {
                    printf("Iteration %d: %.18f\n", iteration_count, global_temperature_change);
                  }
                        
		}

		// Calculate the total time spent processing
		if(my_rank == MASTER_PROCESS_RANK)
		{
			total_time_so_far = MPI_Wtime() - start_time;
		}

		// Send total timer to everybody so they too can exit the loop if more than the allowed runtime has elapsed already
		MPI_Bcast(&total_time_so_far, 1, MPI_DOUBLE, MASTER_PROCESS_RANK, MPI_COMM_WORLD);

		// Update the iteration number
		iteration_count++;
	}

	///////////////////////////////////////////////
	//     ^                                     //
	//    / \                                    //
	//   / | \    CODE FROM HERE IS NOT TIMED    //
	//  /  o  \                                  //
	// /_______\                                 //
	///////////////////////////////////////////////

	/////////////////////////////////////////
	// -- FINALISATION 2: PRINT SUMMARY -- //
	/////////////////////////////////////////
	if(my_rank == MASTER_PROCESS_RANK)
	{
		printf("The program took %5.2f seconds in total and executed %d iterations.\n", total_time_so_far, iteration_count);
	}

	MPI_Finalize();

	return EXIT_SUCCESS;
}
