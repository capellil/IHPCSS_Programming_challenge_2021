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

#define IHPCSS_PATH argv[1]
#define INPUT_FILE argv[2]

#define NEXT_PRINT_INCREASE 10
#define MAX_PATH_LENGTH 256

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
 * @argv[1] path to the cloned Github repository
 * @argv[2] path to the dataset to load
 **/
int main(int argc, char* argv[])
{
	// Check the arguments passed
	if(argc != 3)
	{
		printf("This program is meant to be 3 passed arguments, not %d, as follows: %s <path to IHPCSS folder> <path to dataset file>.\n", argc - 1, argv[1]);
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

	double printing_timer_total = 0.0;
	double printing_timer_start = 0.0;
	double printing_timer_end = 0.0;

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
	if(my_rank == MASTER_PROCESS_RANK)
	{
		printf("There are %d MPI processes.\n", comm_size);
	}

	/// Rank of the first MPI process
	const int FIRST_PROCESS_RANK = 0;
	/// Rank of the last MPI process
	const int LAST_PROCESS_RANK = comm_size - 1;

	// Rank of my up neighbour if any
	int up_neighbour_rank = (my_rank == FIRST_PROCESS_RANK) ? MPI_PROC_NULL : my_rank - 1;
	
	// Rank of my down neighbour if any
	int down_neighbour_rank = (my_rank == LAST_PROCESS_RANK) ? MPI_PROC_NULL : my_rank + 1;

	////////////////////////////////////
	// -- PART 1: DATA ACQUISITION -- //
	////////////////////////////////////
	
	// Wait for all MPI process and launch timer
	MPI_Barrier(MPI_COMM_WORLD);
	acquisition_timer_start = MPI_Wtime();

	/// Array that will contain my part chunk. It will include the 2 ghost rows (1 up, 1 down)
	double* temperatures;
	/// Temperatures from the previous iteration, same dimensions as the array above.
	double* temperatures_last;

	// The master MPI process will read a chunk from the file, send it to the corresponding MPI process and repeat until all chunks are read.
	if(my_rank == MASTER_PROCESS_RANK)
	{
		// Open the file
		char path[MAX_PATH_LENGTH];
		snprintf(path, MAX_PATH_LENGTH, "%s/%s", IHPCSS_PATH, INPUT_FILE);
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

		// Send those dimensions to everybody so they can initialise too.
		MPI_Bcast(&ROWS, 1, MPI_UINT32_T, 0, MPI_COMM_WORLD);
		MPI_Bcast(&COLUMNS, 1, MPI_UINT32_T, 0, MPI_COMM_WORLD);

		// Every MPI process receives 1/nth of the rows. The datasets have been designed to conveniently have a number of rows that is a multiple of the number of MPI processes.
		ROWS_PER_MPI_PROCESS = ROWS / comm_size;
		COLUMNS_PER_MPI_PROCESS = COLUMNS;

		// We can now allocate our array, including 2 extra rows for the halo (1 up, 1 down)
		temperatures = (double*)malloc(sizeof(double) * (ROWS_PER_MPI_PROCESS + 2) * COLUMNS);
		temperatures_last = (double*)calloc((ROWS_PER_MPI_PROCESS + 2) * COLUMNS_PER_MPI_PROCESS, sizeof(double));
		
		// Allocate the temporary buffer that will contain the data read from the file, before we send it to the corresponding MPI process
		double* temp_buffer = (double*)malloc(sizeof(double) * ROWS_PER_MPI_PROCESS * COLUMNS);

		// Start reading the n chunks from the file, one per MPI process.
		int number_of_cells_read;
		for(int i = 0; i < comm_size; i++)
		{
			// Read a chunk and send it to the corresponding MPI process
			number_of_cells_read = fread(temp_buffer, sizeof(double), ROWS_PER_MPI_PROCESS * COLUMNS_PER_MPI_PROCESS, f);
			if(number_of_cells_read != ROWS_PER_MPI_PROCESS * COLUMNS_PER_MPI_PROCESS)
			{
				printf("%d should have been read from the file, but %d were read instead. This should not have happened, abort.\n", ROWS_PER_MPI_PROCESS * COLUMNS_PER_MPI_PROCESS, number_of_cells_read); 
				MPI_Abort(MPI_COMM_WORLD, EXIT_FAILURE);
			}

			// Is this chunk meant for us or another MPI process?
			if(i != my_rank)
			{
				// Chunk meant for another MPI process so we send it.
				MPI_Send(temp_buffer, ROWS_PER_MPI_PROCESS * COLUMNS_PER_MPI_PROCESS, MPI_DOUBLE, i, 0, MPI_COMM_WORLD);
			}
			else
			{
				// This chunk is for us. We copy it into our temperature array.
				for(int j = 1; j <= ROWS_PER_MPI_PROCESS; j++)
				{
					for(int k = 0; k < COLUMNS_PER_MPI_PROCESS; k++)
					{
						temperatures_last[from_2d_index(j,k)] = temp_buffer[from_2d_index(j-1,k)];
					}
				}
			}
		}

		// We are done with the file so we can close it
		fclose(f);

		// We are done with the temporary buffer so we can free it
		free(temp_buffer);
	}
	else
	{
		// Receive dimensions from master
		MPI_Bcast(&ROWS, 1, MPI_UINT32_T, 0, MPI_COMM_WORLD);
		MPI_Bcast(&COLUMNS, 1, MPI_UINT32_T, 0, MPI_COMM_WORLD);
	
		// Every MPI process is in charge of 1/nth of the rows. This has been designed to conveniently have a number of rows that is a multiple of the number of MPI processes.
		ROWS_PER_MPI_PROCESS = ROWS / comm_size;
		COLUMNS_PER_MPI_PROCESS = COLUMNS;
		
		// We can now allocate our array, including 2 extra rows for the halo (1 up, 1 down)
		temperatures = (double*)malloc(sizeof(double) * (ROWS_PER_MPI_PROCESS + 2) * COLUMNS_PER_MPI_PROCESS);
		temperatures_last = (double*)calloc((ROWS_PER_MPI_PROCESS + 2) * COLUMNS_PER_MPI_PROCESS, sizeof(double));

		// Receive my chunk.
		MPI_Recv(&temperatures_last[from_2d_index(1, 0)], ROWS_PER_MPI_PROCESS * COLUMNS_PER_MPI_PROCESS, MPI_DOUBLE, MASTER_PROCESS_RANK, MPI_ANY_TAG, MPI_COMM_WORLD, MPI_STATUS_IGNORE);
	}

	if(my_rank == MASTER_PROCESS_RANK)
	{
		printf("Data acquisition complete.\n");
	}

	////////////////////////
	// Initial halo swap //
	//////////////////////
	
	// We now have our initial data but we are still missing the halos to begin the first iteration. Therefore, an initial halo swap is done.
	// Send data to up neighbour for its ghost cells. If my up_neighbour_rank is MPI_PROC_NULL, this MPI_Send will do nothing.
	MPI_Send(&temperatures_last[from_2d_index(1, 0)], COLUMNS_PER_MPI_PROCESS, MPI_DOUBLE, up_neighbour_rank, 0, MPI_COMM_WORLD);
	// Receive data from down neighbour to fill our ghost cells. If my down_neighbour_rank is MPI_PROC_NULL, this MPI_Recv will do nothing.
	MPI_Recv(&temperatures_last[from_2d_index(ROWS_PER_MPI_PROCESS+1, 0)], COLUMNS_PER_MPI_PROCESS, MPI_DOUBLE, down_neighbour_rank, MPI_ANY_TAG, MPI_COMM_WORLD, MPI_STATUS_IGNORE);
	// Send data to down neighbour for its ghost cells. If my down_neighbour_rank is MPI_PROC_NULL, this MPI_Send will do nothing.
	MPI_Send(&temperatures_last[from_2d_index(ROWS_PER_MPI_PROCESS, 0)], COLUMNS_PER_MPI_PROCESS, MPI_DOUBLE, down_neighbour_rank, 0, MPI_COMM_WORLD);
	// Receive data from up neighbour to fill our ghost cells. If my up_neighbour_rank is MPI_PROC_NULL, this MPI_Recv will do nothing.
	MPI_Recv(&temperatures_last[from_2d_index(0, 0)], COLUMNS_PER_MPI_PROCESS, MPI_DOUBLE, up_neighbour_rank, MPI_ANY_TAG, MPI_COMM_WORLD, MPI_STATUS_IGNORE);

	// Wait for everybody to receive their part before we can start processing
	MPI_Barrier(MPI_COMM_WORLD);
	acquisition_timer_end = MPI_Wtime();
	acquisition_timer_total = acquisition_timer_end - acquisition_timer_start;
	
	/////////////////////////////
	// PART 2: DATA PROCESSING //
	/////////////////////////////
	int iteration_count = 0;
	/// Maximum temperature change observed across all MPI processes
	double global_temperature_change;
	/// Maximum temperature change for us
	double my_temperature_change; 

	if(my_rank == MASTER_PROCESS_RANK)
	{
		processing_timer_start = MPI_Wtime();
	}

	while(processing_timer_total < 60.0)
	{
		my_temperature_change = 0.0;

		// Calculating the average of neighbouring temperatures for each cell
		for(int i = 1; i <= ROWS_PER_MPI_PROCESS; i++)
		{
			// Process the cell at the first column, which has no left neighbour
			if(temperatures[from_2d_index(i, 0)])
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

		// Calculate the variation in temperatures
		for(int i = 1; i <= ROWS_PER_MPI_PROCESS; i++)
		{
			for(int j = 0; j < COLUMNS_PER_MPI_PROCESS; j++)
			{
				my_temperature_change = fmax(fabs(temperatures[from_2d_index(i, j)] - temperatures_last[from_2d_index(i, j)]), my_temperature_change);
			}
		}

		// Exchange ghost cells
		// Send data to up neighbour for its ghost cells. If my up_neighbour_rank is MPI_PROC_NULL, this MPI_Send will do nothing.
		MPI_Send(&temperatures[from_2d_index(1, 0)], COLUMNS_PER_MPI_PROCESS, MPI_DOUBLE, up_neighbour_rank, 0, MPI_COMM_WORLD);
		
		// Receive data from down neighbour to fill our ghost cells. If my down_neighbour_rank is MPI_PROC_NULL, this MPI_Recv will do nothing.
		MPI_Recv(&temperatures_last[from_2d_index(ROWS_PER_MPI_PROCESS+1, 0)], COLUMNS_PER_MPI_PROCESS, MPI_DOUBLE, down_neighbour_rank, MPI_ANY_TAG, MPI_COMM_WORLD, MPI_STATUS_IGNORE);

		// Send data to down neighbour for its ghost cells. If my down_neighbour_rank is MPI_PROC_NULL, this MPI_Send will do nothing.
		MPI_Send(&temperatures[from_2d_index(ROWS_PER_MPI_PROCESS, 0)], COLUMNS_PER_MPI_PROCESS, MPI_DOUBLE, down_neighbour_rank, 0, MPI_COMM_WORLD);

		// Receive data from up neighbour to fill our ghost cells. If my up_neighbour_rank is MPI_PROC_NULL, this MPI_Recv will do nothing.
		MPI_Recv(&temperatures_last[from_2d_index(0, 0)], COLUMNS_PER_MPI_PROCESS, MPI_DOUBLE, up_neighbour_rank, MPI_ANY_TAG, MPI_COMM_WORLD, MPI_STATUS_IGNORE);
			
		// Calculate the biggest temperature delta we've had
		// Obstacle: this is a reduction in boomerang
		if(my_rank != MASTER_PROCESS_RANK)
		{
			// Send my temperature delta to the master MPI process
			MPI_Send(&my_temperature_change, 1, MPI_DOUBLE, MASTER_PROCESS_RANK, 0, MPI_COMM_WORLD);
			
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
					MPI_Send(&global_temperature_change, 1, MPI_DOUBLE, j, 0, MPI_COMM_WORLD);
				}
			}
		}

		// Copy current temperatures into last temperatures
		for(int i = 1; i <= ROWS_PER_MPI_PROCESS; i++)
		{
			for(int j = 0; j < COLUMNS_PER_MPI_PROCESS; j++)
			{
				temperatures_last[from_2d_index(i,j)] = temperatures[from_2d_index(i,j)];
			}
		}

		// Print progress
		if(iteration_count == next_print)
		{
			if(my_rank == MASTER_PROCESS_RANK)
			{
				printing_timer_start = MPI_Wtime();
				double* global_temperatures = (double*)malloc(sizeof(double) * ROWS * COLUMNS);

				for(int j = 0; j < comm_size; j++)
				{
					if(j == my_rank)
					{
						// Copy locally my own temperature array in the global one
						//memcpy(&global_temperatures[from_2d_index(j * ROWS_PER_MPI_PROCESS, 0)], &temperatures[from_2d_index(1, 0)], sizeof(double) * ROWS_PER_MPI_PROCESS * COLUMNS_PER_MPI_PROCESS);
						for(int k = 0; k < ROWS_PER_MPI_PROCESS; k++)
						{
							for(int l = 0; l < COLUMNS_PER_MPI_PROCESS; l++)
							{
								global_temperatures[from_2d_index(j * ROWS_PER_MPI_PROCESS + k, l)] = temperatures[from_2d_index(k + 1, l)];
							}
						}
					}
					else
					{
						MPI_Recv(&global_temperatures[from_2d_index(j * ROWS_PER_MPI_PROCESS, 0)], ROWS_PER_MPI_PROCESS * COLUMNS_PER_MPI_PROCESS, MPI_DOUBLE, j, MPI_ANY_TAG, MPI_COMM_WORLD, MPI_STATUS_IGNORE);
					}
				}

				// Check that no temperature is above the max temperature
				for(int j = 0; j < ROWS; j++)
				{
					for(int k = 0; k < COLUMNS; k++)
					{
						if(global_temperatures[from_2d_index(j,k)] > MAX_TEMPERATURE)
						{
							printf("At iteration %d, the cell at [%d,%d] has a temperature of %f, which is beyond %f.\n", next_print, j, k, global_temperatures[from_2d_index(j,k)], MAX_TEMPERATURE);
							MPI_Abort(MPI_COMM_WORLD, EXIT_FAILURE);
						}
					}
				}

				// Open PPM file to store the corresponding image
				char path[MAX_PATH_LENGTH];
				snprintf(path, MAX_PATH_LENGTH, "%s/output/%d.ppm", IHPCSS_PATH, next_print);
				FILE* ppm_file = fopen(path, "wb");
				if(ppm_file == NULL)
				{
					printf("Could not create the PPM file %s.\n", path);
					return EXIT_FAILURE;
				}

				// Write header
				fprintf(ppm_file, "P6 %d %d 255 ", ROWS, COLUMNS);

				// Convert temperatures to colours
				uint8_t* colours = (uint8_t*)malloc(sizeof(uint8_t) * ROWS * COLUMNS * 3);
				for(int j = 0; j < ROWS * COLUMNS * 3; j+=3)
				{
					colours[j  ] = (uint8_t)(global_temperatures[j/3] / MAX_TEMPERATURE * 255.0);
					colours[j+1] = 0;
					colours[j+2] = (uint8_t)((1.0 - (global_temperatures[j/3] / MAX_TEMPERATURE)) * 255.0);
				}

				// Write data
				fwrite(colours, sizeof(uint8_t), ROWS * COLUMNS * 3, ppm_file);
				fclose(ppm_file);

				free(colours);

				printf("PPM generated for iteration %d.\n", next_print);

				printing_timer_end = MPI_Wtime();
				printing_timer_total += printing_timer_end - printing_timer_start;
			}
			else
			{
				// Send my array to the master MPI process
				MPI_Send(&temperatures[from_2d_index(1, 0)], ROWS_PER_MPI_PROCESS * COLUMNS_PER_MPI_PROCESS, MPI_DOUBLE, MASTER_PROCESS_RANK, 0, MPI_COMM_WORLD); 
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

		iteration_count++;

		// Calculate the total time spent processing
		if(my_rank == MASTER_PROCESS_RANK)
		{
			processing_timer_end = MPI_Wtime();
			processing_timer_total = processing_timer_end - processing_timer_start;
		}

		// Send total timer to everybody so they too can exit the loop if more than the allowed runtime has elapsed already
		MPI_Bcast(&processing_timer_total, 1, MPI_DOUBLE, MASTER_PROCESS_RANK, MPI_COMM_WORLD);
	}

	// We are done with the temperature arrays, we can free them.
	free(temperatures);
	free(temperatures_last);

	// Print a little summary
	if(my_rank == MASTER_PROCESS_RANK)
	{
		printf("The program took %f seconds in total.\n", acquisition_timer_total + processing_timer_total);
		printf("\t- %f seconds on acquisition.\n", acquisition_timer_total);
		printf("\t- %f seconds on processing\n", processing_timer_total);
		printf("\t\t- %f seconds were spent on printing\n", printing_timer_total);
		printf("\t\t- %d iterations were run\n", iteration_count);
	}

	MPI_Finalize();

	return EXIT_SUCCESS;
}
