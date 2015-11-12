#include <stdio.h>
#include <stdlib.h>
#include <string.h>


void  readFile(char **filename, double *fare)
{
	FILE* csvFile = fopen(*filename, "r");
	size_t buf_size = 100;
	char* buf = (char*)malloc(buf_size);
	char* token = NULL;

	int k = 0;
	double total_amount = 0, tolls_amount = 0;


	if (csvFile)   //whether the file is opend properly
	{
		getline(&buf, &buf_size, csvFile);

		while (getline(&buf, &buf_size, csvFile) != -1)
		{
			char* buf2 = buf;

			//token = strtok(buf, ",");
			//if (token == NULL) continue;
			for (int i = 0; i < 10; i++)
			{
				//token = strtok(NULL, ",");
				token = strsep(&buf2, ",");
			}
			tolls_amount = atof(token);
			//token = strtok(NULL, "\n");
			token = strsep(&buf2, "\n");
			total_amount = atof(token);

			fare[k++] = total_amount - tolls_amount;
		}
		fclose(csvFile);
		free(buf);

	}
}

void  readFile2(char **filename, double *sec)   
{
	FILE* csvFile = fopen(*filename, "r");
	size_t buf_size = 100;
	char* buf = (char*)malloc(buf_size);
	char* token = NULL;

	int k = 0;


	if (csvFile)   //whether the file is opend properly
	{
		getline(&buf, &buf_size, csvFile);

		while (getline(&buf, &buf_size, csvFile) != -1)
		{
			char* buf2 = buf;

			//token = strtok(buf, ",");
			//if (token == NULL) continue;
			for (int i = 0; i < 9; i++)
			{
				//token = strtok(NULL, ",");
				token = strsep(&buf2, ",");
			}
			sec[k++] = atof(token);
		}
		fclose(csvFile);
		free(buf);

	}
}

void  readFile3(char **filename, double *surcharge)
{
	FILE* csvFile = fopen(*filename, "r");
	size_t buf_size = 100;
	char* buf = (char*)malloc(buf_size);
	char* token = NULL;

	int k = 0;


	if (csvFile)   //whether the file is opend properly
	{
		getline(&buf, &buf_size, csvFile);

		while (getline(&buf, &buf_size, csvFile) != -1)
		{
			char* buf2 = buf;

			//token = strtok(buf, ",");
			//if (token == NULL) continue;
			for (int i = 0; i < 7; i++)
			{
				//token = strtok(NULL, ",");
				token = strsep(&buf2, ",");
			}
			surcharge[k++] = atof(token);
		}
		fclose(csvFile);
		free(buf);

	}
}