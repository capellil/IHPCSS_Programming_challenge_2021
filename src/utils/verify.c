#include <stdio.h>
#include <stdlib.h>
#include <stdbool.h>
#include <string.h>
#include <inttypes.h>

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
	if(argc != 2)
	{
		printf("This program is meant to be run with 1 arguments: %s <ppm file to verify>.\n", argv[0]);
		return EXIT_FAILURE;
	}

	// Open the file to verify
	FILE* file_to_verify = fopen(argv[1], "rb");
	if(file_to_verify == NULL)
	{
		printf("Could not open the file to verify.\n");
		return EXIT_FAILURE;
	}

	// Read the header
	int ROWS = 0;
	int COLUMNS = 0;
	fscanf(file_to_verify, "P6 %d %d 255\n", &ROWS, &COLUMNS);

	// Read the metadata
	char language = '\0';
	int snapshot_iteration = 0;
	fscanf(file_to_verify, "# %c %d\n", &language, &snapshot_iteration);

	// Print a little summary of the file we are opening
	printf("This file is a %s snapshot of %dx%d taken at the end of iteration %d.\n", (language == 'C') ? "C" : "FORTRAN", ROWS, COLUMNS, snapshot_iteration);

	const int PATH_MAX_LENGTH = 512;
	char path[PATH_MAX_LENGTH];
	snprintf(path, PATH_MAX_LENGTH, "/jet/home/capellil/IHPCSS_Programming_challenge_reference/%c/%dx%d/%d.ppm", language, ROWS, COLUMNS, snapshot_iteration);
	printf("The reference file will be \"%s\".\n", path);

	FILE* reference_file = fopen(path, "rb");
	if(reference_file == NULL)
	{
		printf("Could not open the reference file.\n");
		fclose(file_to_verify);
		return EXIT_FAILURE;
	}

	printf("Reference file found and successfully opened.\n");

	// Read the header
	fscanf(reference_file, "P6 %d %d 255\n", &ROWS, &COLUMNS);

	// Read the metadata
	fscanf(reference_file, "# %c %d\n", &language, &snapshot_iteration);

	// Load both files in memory
	uint8_t* colours_to_verify = (uint8_t*)malloc(sizeof(uint8_t) * ROWS * COLUMNS * 3);
	uint8_t* colours_reference = (uint8_t*)malloc(sizeof(uint8_t) * ROWS * COLUMNS * 3);

	fread(colours_to_verify, sizeof(uint8_t), ROWS * COLUMNS * 3, file_to_verify);
	fread(colours_reference, sizeof(uint8_t), ROWS * COLUMNS * 3, reference_file);

	bool identical = true;
	for(int i = 0; i < ROWS * COLUMNS * 3 && identical; i++)
	{
		if(colours_to_verify[i] != colours_reference[i])
		{
			printf("Colours differ at location %d: %u (yours) vs %u (reference)\n", i, colours_to_verify[i], colours_reference[i]);
			identical = false;
		}
	}
	
	if(identical)
	{
		printf("The file to verify is identical to the reference file. Well done.\n");
	}

	// Close the reference file
	fclose(reference_file);

	// Close the file to verify
	fclose(file_to_verify);

	return EXIT_SUCCESS;
}

int from_2d_index(int i, int j)
{
	return i * COLUMNS + j;
}
