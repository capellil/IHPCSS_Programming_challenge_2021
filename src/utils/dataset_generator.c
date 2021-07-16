#include <stdio.h>
#include <stdlib.h>
#include <stdbool.h>
#include <string.h>

int ROWS;
int COLUMNS; 

int from_2d_index(int i, int j);

/**
 * @brief Generates a dataset for the challenge binaries.
 * @param This program can produce files in 2 different formats
 * and 2 different sizes. The two formats are:
 * - C format: respecting column-major order
 * - FORTRAN format: respecting row-major order
 * The 2 sizes are:
 * - Sample size: small dataset used for testing and quick runs.
 * - Challenge size: big dataset representing study case and used in final assessment.
 *
 * This program therefore accepts 2 parameters:
 * 1) The format to respect: either 'C' or 'FORTRAN'.
 * 2) The size of the dataset: either 'sample' or 'challenge'.
 **/
int main(int argc, char* argv[])
{
	if(argc != 3)
	{
		printf("This program is meant to be run with 2 arguments: %s <dataset format> <dataset size>.\n", argv[0]);
		printf("\t- <dataset format> can be either 'C' or 'FORTRAN'.\n");
		printf("\t- <dataset size> can be either 'sample' or 'challenge'.\n");
		return EXIT_FAILURE;
	}

	bool c_format = false;
	bool sample_size = false;

	// Check format
	if(strcmp(argv[1], "C") == 0)
	{
		c_format = true;
	}
	else if(strcmp(argv[1], "FORTRAN") == 0)
	{
		c_format = false;
	}
	else
	{
		printf("The format was not understood. It was \"%s\" but only \"C\" and \"FORTRAN\" are accepted.\n", argv[1]);
		return EXIT_FAILURE;
	}

	// Check dataset size
	if(strcmp(argv[2], "sample") == 0)
	{
		sample_size = true;
	}
	else if(strcmp(argv[2], "challenge") == 0)
	{
		sample_size = false;
	}
	else
	{
		printf("The dataset size was not understood. It was \"%s\" but only \"sample\" and \"challenge\" are accepted.\n", argv[2]);
		return EXIT_FAILURE;
	}

	// If we reached this point, both the format and the dataset sizes are valid.
	printf("IHPCSS folder is %s.\n", IHPCSS_FOLDER);

	ROWS = (sample_size) ? 512 : 15360;
	COLUMNS = ROWS;

	double* temperatures = (double*)malloc(sizeof(double) * ROWS * COLUMNS);
	if(temperatures == NULL)
	{
		printf("Could not allocated an array of %d x %d doubles.\n", ROWS, COLUMNS);
		return EXIT_FAILURE;
	}
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
				temperatures[from_2d_index(i, j)] = MAX_TEMPERATURE;
			}
			else
			{
				temperatures[from_2d_index(i, j)] = 0.0;
			}
			if(temperatures[from_2d_index(i, j)] > MAX_TEMPERATURE)
			{
				printf("Cell at [%d,%d] has a temperature of %f, which is beyond %f.\n", i, j, temperatures[from_2d_index(i, j)], MAX_TEMPERATURE);
				return EXIT_FAILURE;
			}
		}
	}

	// Write all temperatures into the corresponding file
	const int NAME_MAX_LENGTH = 256;
	char name[NAME_MAX_LENGTH];
	snprintf(name, NAME_MAX_LENGTH, "%s/datasets/%s_%s.dat", IHPCSS_FOLDER, argv[1], argv[2]);
	printf("Saving the dataset at %s.\n", name);
	FILE* f = fopen(name, "wb");
	if(f == NULL)
	{
		printf("Could not open the file at %s.\n", name);
		return EXIT_FAILURE;
	}
	if(fwrite(&ROWS, sizeof(int), 1, f) != 1)
	{
		printf("The writing of the number of the rows (%d) in the file failed.\n", ROWS);
		return EXIT_FAILURE;
	}
	if(fwrite(&COLUMNS, sizeof(int), 1, f) != 1)
	{
		printf("The writing of the number of columns (%d) in the file failed.\n", COLUMNS);
		return EXIT_FAILURE;
	}
	if(fwrite(temperatures, sizeof(double), ROWS * COLUMNS, f) != (size_t)(ROWS * COLUMNS))
	{
		printf("The writing of the %d cells in the file failed.\n", ROWS * COLUMNS);
		return EXIT_FAILURE;
	}
	long unsigned file_size = ftell(f);
	fclose(f);
	free(temperatures);

	printf("Dataset successfully generated:\n");
	printf("\t- In %s format\n", (c_format) ? "C" : "FORTRAN");
	printf("\t- In %s size (ROWS: %d, COLUMNS: %d)\n", (sample_size) ? "sample" : "challenge", ROWS, COLUMNS);
	printf("\t- Size of the file generated: %lu bytes\n", file_size);

	return EXIT_SUCCESS;
}

int from_2d_index(int i, int j)
{
	return i * COLUMNS + j;
}
