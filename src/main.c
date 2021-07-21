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

#define INPUT_FILE argv[1]
#define OUTPUT_DIRECTORY "outputs"

#define NEXT_PRINT_INCREASE 10
#define MAX_PATH_LENGTH 256
#define MAX_TIME 30.0

/// Total number of rows in the data to process (no ghost cells)
int ROWS = 0;
/// Total number of columns in the data to process (no ghost cells)
int COLUMNS = 0;
/// The number of rows per MPI process
int ROWS_PER_MPI_PROCESS = 0;
/// The number of columns per MPI process
int COLUMNS_PER_MPI_PROCESS = 0;

/**
 * @brief Converts a 2D coordinate into a 1D coordinate.
 **/
inline int from_2d_index(int row_index, int column_index)
{
	return row_index * COLUMNS + column_index;
}

/**
 * @argv[0] Name of the program
 * @argv[1] path to the dataset to load
 **/
int main(int argc, char* argv[])
{
	// Check the arguments passed
	if(argc != 2)
	{
		printf("This program is meant to be passed 1 argument, not %d, as follows: %s <dataset file>.\n", argc - 1, argv[0]);
		return EXIT_FAILURE;
	}

	MPI_Init(NULL, NULL);

	/////////////////////////////////
	// -- PREPARATION 1: TIMERS -- //
	/////////////////////////////////
	double acquisition_timer_total = 0.0;
	double acquisition_timer_start = 0.0;
	double acquisition_timer_end = 0.0;

	double processing_timer_total = 0.0;
	double processing_timer_start = 0.0;
	double processing_timer_end = 0.0;

	double halo_swap_timer_total = 0.0;
	double halo_swap_timer_start = 0.0;
	double halo_swap_timer_end = 0.0;

	double pure_processing_timer_start = 0.0;
	double pure_processing_timer_end = 0.0;
	double pure_processing_timer_total = 0.0;

	double printing_timer_total = 0.0;
	double printing_timer_start = 0.0;
	double printing_timer_end = 0.0;

	double total_time_so_far = 0.0;

	/////////////////////////////////////////////////////
	// -- PREPARATION 2: COLLECT USEFUL INFORMATION -- //
	/////////////////////////////////////////////////////
	/// Contains the next iteration at which we will print progress
	int next_print = 0;
	
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

	double report_start = MPI_Wtime();
	if(my_rank == MASTER_PROCESS_RANK)
	{
		report_start = MPI_Wtime();
	}
	report_placement();
	if(my_rank == MASTER_PROCESS_RANK)
	{
		printf("Took %f seconds to report placement.\n", MPI_Wtime() - report_start);
		printf("There are %d MPI processes.\n", comm_size);
		printf("There are %s OpenMP threads per MPI process.\n", getenv("OMP_NUM_THREADS"));
	}

	//////////////////////////////////////////////
	// -- PREPARATION 3: LOAD DATA FROM FILE -- //
	//////////////////////////////////////////////

	/// Array that will contain my part chunk. It will include the 2 ghost rows (1 up, 1 down)
	double* temperatures = NULL;
	/// Temperatures from the previous iteration, same dimensions as the array above.
	double* temperatures_last = NULL;
	/// On master process only: contains all temperatures read from input file.
	double* all_temperatures_from_file = NULL;

	// The master MPI process will read a chunk from the file, send it to the corresponding MPI process and repeat until all chunks are read.
	if(my_rank == MASTER_PROCESS_RANK)
	{
		// Open the file
		char path[MAX_PATH_LENGTH];
		snprintf(path, MAX_PATH_LENGTH, "%s/%s", IHPCSS_FOLDER, INPUT_FILE);
		FILE* f = fopen(INPUT_FILE, "r");
		if(f == NULL)
		{
			// Could not open the file for a certain reason. Typically, the file does not exist.
			printf("Could not open the file %s.\n", INPUT_FILE);
			MPI_Abort(MPI_COMM_WORLD, EXIT_FAILURE);
		}
		else
		{
			printf("File %s successfully opened.\n", INPUT_FILE);
		}

		// Read dimensions of the mesh
		fread(&ROWS, sizeof(uint32_t), 1, f);
		fread(&COLUMNS, sizeof(uint32_t), 1, f);

		printf("Dimensions read: %u rows x %u columns.\n", ROWS, COLUMNS);
	}

	// Send those dimensions to everybody so they can initialise too.
	MPI_Bcast(&ROWS, 1, MPI_UINT32_T, 0, MPI_COMM_WORLD);
	MPI_Bcast(&COLUMNS, 1, MPI_UINT32_T, 0, MPI_COMM_WORLD);

	// Every MPI process receives 1/nth of the rows. The datasets have been designed to conveniently have a number of rows that is a multiple of the number of MPI processes.
	ROWS_PER_MPI_PROCESS = ROWS / comm_size;
	COLUMNS_PER_MPI_PROCESS = COLUMNS;

	// We can now allocate our array, including 2 extra rows for the halo (1 up, 1 down)
	temperatures = (double*)malloc(sizeof(double) * (ROWS_PER_MPI_PROCESS + 2) * COLUMNS_PER_MPI_PROCESS);
	temperatures_last = (double*)calloc((ROWS_PER_MPI_PROCESS + 2) * COLUMNS_PER_MPI_PROCESS, sizeof(double));
	
	if(my_rank == MASTER_PROCESS_RANK)
	{
		// Allocate the temporary buffer that will contain the data read from the file, before we send it to the corresponding MPI process
		all_temperatures_from_file = (double*)malloc(sizeof(double) * ROWS * COLUMNS);
		if(all_temperatures_from_file == NULL)
		{
			printf("Failure in allocating the buffer to read the entire file at once.\n");
			MPI_Abort(MPI_COMM_WORLD, EXIT_FAILURE);
		}

		// Start reading the n chunks from the file, one per MPI process.
		int number_of_cells_read = fread(all_temperatures_from_file, sizeof(double), ROWS * COLUMNS, f);
		if(number_of_cells_read != ROWS * COLUMNS)
		{
			printf("%d should have been read from the file, but %d were read instead. This should not have happened, abort.\n", ROWS_PER_MPI_PROCESS * COLUMNS_PER_MPI_PROCESS, number_of_cells_read); 
			MPI_Abort(MPI_COMM_WORLD, EXIT_FAILURE);
		}

		// We are done with the file so we can close it
		fclose(f);
		printf("File loading complete.\n");
	}

	MPI_Barrier(MPI_COMM_WORLD);

	///////////////////////////////////////////
	//     ^                                 //
	//    / \                                //
	//   / | \    CODE FROM HERE IS TIMED    //
	//  /  o  \                              //
	// /_______\                             //
	///////////////////////////////////////////
	
	// /////////////////////////////////////////////////////
	// -- TASK 1: DISTRIBUTE DATA TO ALL MPI PROCESSES -- //
	// /////////////////////////////////////////////////////
	acquisition_timer_start = MPI_Wtime();

	if(my_rank == MASTER_PROCESS_RANK)
	{
		for(int i = 0; i < comm_size; i++)
		{
			// Is the i'th chunk meant for me, the master MPI process?
			if(i != my_rank)
			{
				// No, so send the corresponding chunk to that MPI process.
				MPI_Ssend(&all_temperatures_from_file[from_2d_index(i * ROWS_PER_MPI_PROCESS, 0)], ROWS_PER_MPI_PROCESS * COLUMNS_PER_MPI_PROCESS, MPI_DOUBLE, i, 0, MPI_COMM_WORLD);
			}
			else
			{
				// Yes, let's copy it straight for the array in which we read the file into.
				for(int j = 1; j <= ROWS_PER_MPI_PROCESS; j++)
				{
					for(int k = 0; k < COLUMNS_PER_MPI_PROCESS; k++)
					{
						temperatures_last[from_2d_index(j,k)] = all_temperatures_from_file[from_2d_index(j-1,k)];
					}
				}
			}
		}

		// We are done with the buffer into which we had loaded the entire file content, so we can free it.
		free(all_temperatures_from_file);
	}
	else
	{
		// Receive my chunk.
		MPI_Recv(&temperatures_last[from_2d_index(1, 0)], ROWS_PER_MPI_PROCESS * COLUMNS_PER_MPI_PROCESS, MPI_DOUBLE, MASTER_PROCESS_RANK, MPI_ANY_TAG, MPI_COMM_WORLD, MPI_STATUS_IGNORE);
	}

	// Copy the temperatures into the current iteration temperature as well
	for(int i = 1; i <= ROWS_PER_MPI_PROCESS; i++)
	{
		for(int j = 0; j < COLUMNS_PER_MPI_PROCESS; j++)
		{
			temperatures[from_2d_index(i,j)] = temperatures_last[from_2d_index(i,j)];
		}
	}

	if(my_rank == MASTER_PROCESS_RANK)
	{
		printf("Data acquisition complete.\n");
	}

	// Wait for everybody to receive their part before we can start processing
	MPI_Barrier(MPI_COMM_WORLD);
	acquisition_timer_end = MPI_Wtime();
	acquisition_timer_total = acquisition_timer_end - acquisition_timer_start;
	
	/////////////////////////////
	// TASK 2: DATA PROCESSING //
	/////////////////////////////
	int iteration_count = 0;
	/// Maximum temperature change observed across all MPI processes
	double global_temperature_change;
	/// Maximum temperature change for us
	double my_temperature_change; 
	/// The last snapshot made
	double* snapshot = (double*)malloc(sizeof(double) * ROWS * COLUMNS);
	/// The iteration at which the last snapshot was made
	int snapshot_iteration = 0;

	if(my_rank == MASTER_PROCESS_RANK)
	{
		processing_timer_start = MPI_Wtime();
	}

	while(total_time_so_far < MAX_TIME)
	{
		my_temperature_change = 0.0;

		// ////////////////////////////////////////
		// -- SUBTASK 1: EXCHANGE GHOST CELLS -- //
		// ////////////////////////////////////////
		halo_swap_timer_start = MPI_Wtime();

		// Send data to up neighbour for its ghost cells. If my up_neighbour_rank is MPI_PROC_NULL, this MPI_Ssend will do nothing.
		MPI_Ssend(&temperatures[from_2d_index(1, 0)], COLUMNS_PER_MPI_PROCESS, MPI_DOUBLE, up_neighbour_rank, 0, MPI_COMM_WORLD);

		// Receive data from down neighbour to fill our ghost cells. If my down_neighbour_rank is MPI_PROC_NULL, this MPI_Recv will do nothing.
		MPI_Recv(&temperatures_last[from_2d_index(ROWS_PER_MPI_PROCESS+1, 0)], COLUMNS_PER_MPI_PROCESS, MPI_DOUBLE, down_neighbour_rank, MPI_ANY_TAG, MPI_COMM_WORLD, MPI_STATUS_IGNORE);

		// Send data to down neighbour for its ghost cells. If my down_neighbour_rank is MPI_PROC_NULL, this MPI_Ssend will do nothing.
		MPI_Ssend(&temperatures[from_2d_index(ROWS_PER_MPI_PROCESS, 0)], COLUMNS_PER_MPI_PROCESS, MPI_DOUBLE, down_neighbour_rank, 0, MPI_COMM_WORLD);

		// Receive data from up neighbour to fill our ghost cells. If my up_neighbour_rank is MPI_PROC_NULL, this MPI_Recv will do nothing.
		MPI_Recv(&temperatures_last[from_2d_index(0, 0)], COLUMNS_PER_MPI_PROCESS, MPI_DOUBLE, up_neighbour_rank, MPI_ANY_TAG, MPI_COMM_WORLD, MPI_STATUS_IGNORE);

		halo_swap_timer_end = MPI_Wtime();
		halo_swap_timer_total += halo_swap_timer_end - halo_swap_timer_start;

		/////////////////////////////////////////////
		// -- SUBTASK 2: PROPAGATE TEMPERATURES -- //
		/////////////////////////////////////////////
		pure_processing_timer_start = MPI_Wtime();
		for(int i = 1; i <= ROWS_PER_MPI_PROCESS; i++)
		{
			// Process the cell at the first column, which has no left neighbour
			if(temperatures[from_2d_index(i, 0)] != MAX_TEMPERATURE)
			{
				temperatures[from_2d_index(i, 0)] = 0.33 * (temperatures_last[from_2d_index(i-1, 0  )] +
															temperatures_last[from_2d_index(i+1, 0  )] +
															temperatures_last[from_2d_index(i  , 1  )]);
			}
			// Process all cells between the first and last columns excluded, which each has both left and right neighbours
			for(int j = 1; j < COLUMNS_PER_MPI_PROCESS - 1; j++)
			{
				if(temperatures[from_2d_index(i, j)] != MAX_TEMPERATURE)
				{
					temperatures[from_2d_index(i, j)] = 0.25 * (temperatures_last[from_2d_index(i-1, j  )] +
																temperatures_last[from_2d_index(i+1, j  )] +
																temperatures_last[from_2d_index(i  , j-1)] +
																temperatures_last[from_2d_index(i  , j+1)]);
				}
			}
			// Process the cell at the last column, which has no right neighbour
			if(temperatures[from_2d_index(i, COLUMNS_PER_MPI_PROCESS - 1)] != MAX_TEMPERATURE)
			{
				temperatures[from_2d_index(i, COLUMNS_PER_MPI_PROCESS - 1)] = 0.33 * (temperatures_last[from_2d_index(i-1, COLUMNS_PER_MPI_PROCESS - 1)] +
																					  temperatures_last[from_2d_index(i+1, COLUMNS_PER_MPI_PROCESS - 1)] +
																					  temperatures_last[from_2d_index(i  , COLUMNS_PER_MPI_PROCESS - 2)]);
			}
		}

		pure_processing_timer_end = MPI_Wtime();
		pure_processing_timer_total += pure_processing_timer_end - pure_processing_timer_start;

		///////////////////////////////////////////////////////
		// -- SUBTASK 3: CALCULATE MAX TEMPERATURE CHANGE -- //
		///////////////////////////////////////////////////////
		my_temperature_change = 0.0;
		for(int i = 1; i <= ROWS_PER_MPI_PROCESS; i++)
		{
			for(int j = 0; j < COLUMNS_PER_MPI_PROCESS; j++)
			{
				my_temperature_change = fmax(fabs(temperatures[from_2d_index(i, j)] - temperatures_last[from_2d_index(i, j)]), my_temperature_change);
			}
		}
	
		//////////////////////////////////////////////////////////
		// -- SUBTASK 4: FIND MAX TEMPERATURE CHANGE OVERALL -- //
		//////////////////////////////////////////////////////////
		if(my_rank != MASTER_PROCESS_RANK)
		{
			// Send my temperature delta to the master MPI process
			MPI_Ssend(&my_temperature_change, 1, MPI_DOUBLE, MASTER_PROCESS_RANK, 0, MPI_COMM_WORLD);
			
			// Receive the total delta calculated by the MPI process based on all MPI processes delta
			MPI_Recv(&global_temperature_change, 1, MPI_DOUBLE, MASTER_PROCESS_RANK, MPI_ANY_TAG, MPI_COMM_WORLD, MPI_STATUS_IGNORE);
		}
		else
		{
			// Initialise the temperature change to mine
			global_temperature_change = my_temperature_change;

			// Pick highest temperature change observed
			for(int j = 0; j < comm_size; j++)
			{
				if(j != my_rank)
				{
					double subtotal;
					MPI_Recv(&subtotal, 1, MPI_DOUBLE, j, MPI_ANY_TAG, MPI_COMM_WORLD, MPI_STATUS_IGNORE);
					if(subtotal > global_temperature_change)
					{
						global_temperature_change = subtotal;
					}
				}
			}

			// Send delta back to all MPI processes
			for(int j = 0; j < comm_size; j++)
			{
				if(j != my_rank)
				{
					MPI_Ssend(&global_temperature_change, 1, MPI_DOUBLE, j, 0, MPI_COMM_WORLD);
				}
			}
		}

		//////////////////////////////////////////////////
		// -- SUBTASK 5: UPDATE LAST ITERATION ARRAY -- //
		//////////////////////////////////////////////////
		for(int i = 1; i <= ROWS_PER_MPI_PROCESS; i++)
		{
			for(int j = 0; j < COLUMNS_PER_MPI_PROCESS; j++)
			{
				temperatures_last[from_2d_index(i,j)] = temperatures[from_2d_index(i,j)];
			}
		}

		///////////////////////////////////
		// -- SUBTASK 6: GET SNAPSHOT -- //
		///////////////////////////////////
		if(iteration_count == next_print)
		{
			if(my_rank == MASTER_PROCESS_RANK)
			{
				printing_timer_start = MPI_Wtime();

				for(int j = 0; j < comm_size; j++)
				{
					if(j == my_rank)
					{
						// Copy locally my own temperature array in the global one
						for(int k = 0; k < ROWS_PER_MPI_PROCESS; k++)
						{
							for(int l = 0; l < COLUMNS_PER_MPI_PROCESS; l++)
							{
								snapshot[from_2d_index(j * ROWS_PER_MPI_PROCESS + k, l)] = temperatures[from_2d_index(k + 1, l)];
							}
						}
					}
					else
					{
						MPI_Recv(&snapshot[from_2d_index(j * ROWS_PER_MPI_PROCESS, 0)], ROWS_PER_MPI_PROCESS * COLUMNS_PER_MPI_PROCESS, MPI_DOUBLE, j, MPI_ANY_TAG, MPI_COMM_WORLD, MPI_STATUS_IGNORE);
					}
				}

				snapshot_iteration = iteration_count;

				printing_timer_end = MPI_Wtime();
				printing_timer_total += printing_timer_end - printing_timer_start;
				printf("Total time so far at iteration %d: %f seconds.\n", iteration_count, total_time_so_far);
			}
			else
			{
				// Send my array to the master MPI process
				MPI_Ssend(&temperatures[from_2d_index(1, 0)], ROWS_PER_MPI_PROCESS * COLUMNS_PER_MPI_PROCESS, MPI_DOUBLE, MASTER_PROCESS_RANK, 0, MPI_COMM_WORLD); 
			}

			// Delay next print by doubling time needed
			if(next_print == 0)
			{
				next_print = 1;
			}
			else
			{
				next_print *= NEXT_PRINT_INCREASE;
			}
		}

		// Calculate the total time spent processing
		if(my_rank == MASTER_PROCESS_RANK)
		{
			processing_timer_end = MPI_Wtime();
			processing_timer_total = processing_timer_end - processing_timer_start;
			total_time_so_far = acquisition_timer_total + processing_timer_total;
		}

		// Send total timer to everybody so they too can exit the loop if more than the allowed runtime has elapsed already
		MPI_Bcast(&total_time_so_far, 1, MPI_DOUBLE, MASTER_PROCESS_RANK, MPI_COMM_WORLD);

		// Update the iteration number
		iteration_count++;
	}

	// We are done with the temperature arrays, we can free them.
	free(temperatures);
	free(temperatures_last);

	///////////////////////////////////////////////
	//     ^                                     //
	//    / \                                    //
	//   / | \    CODE FROM HERE IS NOT TIMED    //
	//  /  o  \                                  //
	// /_______\                                 //
	///////////////////////////////////////////////
	
	/////////////////////////////////////////
	// -- FINALISATION 1: DUMP SNAPSHOT -- //
	/////////////////////////////////////////
	if(my_rank == MASTER_PROCESS_RANK)
	{
		uint8_t* colours = (uint8_t*)malloc(sizeof(uint8_t) * ROWS * COLUMNS * 3);

		// Open PPM file to store the corresponding image
		char path[MAX_PATH_LENGTH];
		snprintf(path, MAX_PATH_LENGTH, "%s/%s/%d.ppm", IHPCSS_FOLDER, OUTPUT_DIRECTORY, snapshot_iteration);
		FILE* ppm_file = fopen(path, "wb");
		if(ppm_file == NULL)
		{
			printf("Could not create the PPM file %s.\n", path);
			MPI_Abort(MPI_COMM_WORLD, EXIT_FAILURE);
		}

		// Write header
		fprintf(ppm_file, "P6 %d %d 255\n", ROWS, COLUMNS);

		// Write the programming language used, the dataset size and the iteration at which that snapshot was taken
		fprintf(ppm_file, "# C %d\n", snapshot_iteration);

		// Convert temperatures to colours
		for(int j = 0; j < ROWS * COLUMNS * 3; j+=3)
		{
			colours[j  ] = (uint8_t)(snapshot[j/3] / MAX_TEMPERATURE * 255.0);
			colours[j+1] = 0;
			colours[j+2] = (uint8_t)((1.0 - (snapshot[j/3] / MAX_TEMPERATURE)) * 255.0);
		}

		// Write data
		fwrite(colours, sizeof(uint8_t), ROWS * COLUMNS * 3, ppm_file);
		fclose(ppm_file);

		printf("Last snapshot generated at iteration %d.\n", snapshot_iteration);
		free(colours);
	}

	/////////////////////////////////////////
	// -- FINALISATION 2: PRINT SUMMARY -- //
	/////////////////////////////////////////
	if(my_rank == MASTER_PROCESS_RANK)
	{
		printf("The program took %5.2f seconds in total and executed %d iterations.\n", acquisition_timer_total + processing_timer_total, iteration_count);
		printf("\t- %5.2f seconds on acquisition.\n", acquisition_timer_total);
		printf("\t- %5.2f seconds on iterations\n", processing_timer_total - printing_timer_total - halo_swap_timer_total - pure_processing_timer_total);
		printf("\t- %5.2f seconds on pure processing.\n", pure_processing_timer_total);
		printf("\t- %5.2f seconds on halo swap\n", halo_swap_timer_total);
		printf("\t- %5.2f seconds on printing\n", printing_timer_total);
	}

	MPI_Finalize();

	return EXIT_SUCCESS;
}
