#include <stdio.h>
#include <stdlib.h>

/*Define a _Matrix structure, assign Matrix to struct _Matrix*/
typedef struct _Matrix
{
	int* element;   /*array of matrix's elements*/
	int row;		/*row of matrix*/
	int col;		/*column of matrix*/
}Matrix;

/*
* Function:  createMatrix_b
* --------------------
* This function is to create a new matrix based on original one. The new matrix can be
* considered as all elements in old matrix move downwards.
* 
* This function is similar to "grid.new = rbind(grid.old[n,], grid.old[1:n-1,])" in R
* version.
*
* grid: Original matrix to create a new matrix
* arr: pointer to head of an array
*  
* return: The new matrix which has Matrix structure.
*/
Matrix createMatrix_b(Matrix grid, int* arr)
{
	Matrix pos;
	int length = grid.col*grid.row;
	int rule,numCol;				/*rule is used to identify whether the cars get to*/
									/*the edge of the grid;numCol represents which column*/
									/*the car is in*/
										

	for (int i = 0; i < length; i++)
	{
		rule = (i + 1) % grid.row;
		numCol = (i + 1) / grid.row;
		if (rule == 0)
			arr[(numCol - 1)*grid.row] = grid.element[i];
		else arr[i + 1] = grid.element[i];
	}/*if car is at the top of the column, we move it to the bottom, otherwise*/
	 /*just move downwards 1 unit*/
	
	pos.element = arr;
	pos.row = grid.row;
	pos.col = grid.col; /*give pos values*/
	return pos;
}
/*
* Function:  movCar_b
* --------------------
* This function is to move blue car. In this function, we can find which cars can move 
* based on difference between two grids firstly. Then move the car according to corresponding
* rules.
*
* grid: Original matrix to represent grid.
* pos: new matrix created by function createMatrix_b
*
* return: The new matrix store the grid int which the blue cars moved one time.
*/
Matrix movCar_b(Matrix grid, Matrix pos)
{
	int length = grid.col * grid.row;
	int flag, rule;		/*flag indicates whether the car can move*/
						/*rule is used to decide whether the car is at the edge of the grid*/
	
	int index;			/*index is used to record the location of the car*/
	
	int * arr_tmp = (int*)malloc(sizeof(int)*length); /*temproary array to record the*/
													  /*change of the location; cannot*/
													  /*set the length of array in C, So*/
													  /*dynamic array is used*/
	for (int i = 0; i < length; i++)
	{
		arr_tmp[i] = grid.element[i];
	}

	
	for (int i = 0; i < length; i++)
	{
		index = i;
		flag = pos.element[i] - grid.element[i];     
		rule = i % grid.row;						
		if (flag == -1) /*car can move when flag == -1*/
		{
			arr_tmp[index] = 0;

			if (rule == 0) index = index + grid.row - 1;
			else index = index - 1;

			arr_tmp[index] = 1;
		}/*move the blue car*/

	}
	for (int i = 0; i < length; i++)
	{
		grid.element[i] = arr_tmp[i];
	}
	free(arr_tmp);
	return grid;
}

/*
* Function:  createMatrix_a
* --------------------
* This function is to create a new matrix based on original one. The new matrix can be
* considered as all elements in old matrix move leftwards.
*
* This function is similar to "grid.new = cbind(grid.old[,2:n], grid.old[,1])" in R
* version.
*
* grid: Original matrix to create a new matrix
* arr: pointer to head of an array
*
* return: The new matrix which has Matrix structure.
*/
Matrix createMatrix_r(Matrix grid, int* arr)
{
	Matrix pos;
	int length = grid.col*grid.row;
	int boundary = length - grid.row;	/*where to move the element in a array*/

	for (int i = 0; i < grid.row; i++)
	{
		arr[boundary + i] = grid.element[i];
	}

	for (int i = grid.row; i < length; i++)
	{
		arr[i - grid.row] = grid.element[i];
	}/*if the car is in the leftmost location of a row, move it to the rightmost place */
	 /*Otherwise, move it leftwards one unit*/
	pos.element = arr;
	pos.row = grid.row;
	pos.col = grid.col;
	return pos;
}
/*
* Function:  movCar_r
* --------------------
* This function is to move red car. In this function, we can find which cars can move
* based on difference between two grids firstly. Then move the car according to corresponding
* rules.
*
* grid: Original matrix to represent grid.
* pos: new matrix created by function createMatrix_r
*
* return: The new matrix store the grid in which the red cars moved one time.
*/
Matrix movCar_r(Matrix grid, Matrix pos)
{
	int length = grid.col * grid.row;
	int flag, rule;		/*flag indicates whether the car can move*/
						/*rule is used to decide whether the car is at the edge of the grid*/
	

	int index, tempPar;
	
	int * arr_tmp = (int*)malloc(sizeof(int)*length);/*temproary array to record the*/
													  /*change of the location; cannot*/
													  /*set the length of array in C, So*/
													  /*dynamic array is used*/
	for (int i = 0; i < length; i++)
	{
		arr_tmp[i] = grid.element[i];
	}

	for (int i = 0; i < length; i++)
	{
		index = i;
		flag = pos.element[i] - grid.element[i];     /*whether the car in this loc can move*/
		tempPar = (grid.col - 1)*grid.row;
		rule = i - tempPar;						/*whehter the car is at the edge of the grid*/
		if (flag == -3)/*car can move when flag==-3*/
		{
			arr_tmp[index] = 0;

			if (rule >= 0) index = index - tempPar;
			else index = index + grid.row;

			arr_tmp[index] = 3;
		}

	}
	
	for (int i = 0; i < length; i++)
	{
		grid.element[i] = arr_tmp[i];
	}
	free(arr_tmp);
	return grid;
}

/*
* Function:  movCar
* --------------------
* In this function, if time is an odd number, then move blue car this step. If time is 
* an even number, then move red car this step.
*
* grid:	matrix to represent grid.
* time:indicate which type of car can move this step
*
* return: The new matrix store the grid after one movement of blue or red cars.
*/
Matrix movCar(Matrix grid, int time)
{
	int length = grid.col*grid.row;
	int * arr_tmp = (int*)malloc(sizeof(int)*length);
	Matrix position;

	if (time % 2 == 1) /*if time%2 == 1, then we move blue car this step*/
	{
		position = createMatrix_b(grid, arr_tmp);
		grid = movCar_b(grid, position);
	}
	else
	{
		position = createMatrix_r(grid, arr_tmp);
		grid = movCar_r(grid, position);
	}
	free(arr_tmp);
	return grid;
}

/*
* Function: runBMLGrid
* ---------------------
* This function can move cars specific number of steps. And this function will be called
* in R.
*
* matrix: array of element in grid
* row: row of the grid
* col: col of the grid
* numStep: number of steps
*/
void runBMLGrid(int* matrix, int* row, int* col, int *numStep)
{
	Matrix grid = { matrix, row[0], col[0] };
	for (int num = 1; num <= numStep[0]; num++){
		grid = movCar(grid, num);
	}
}


/*Use main function to test all functions above work fine*/
/*void main(void)
{
	int b[12] = { 3, 0, 0, 0, 1, 0, 1, 0, 0, 0, 3, 0 };
	int row[1] = { 3 };
	int col[1] = { 4 };
	int numStep[1] = { 2 };

	runBMLGrid(b, row, col, numStep);
	for (int i = 0; i < 12; i++){
		printf("%d", b[i]);
	}
}
*/