!>
!> @file main.c
!> @brief This file contains the source code of the application to parallelise.
!> @details This application is a classic heat spread simulation.
!> @author Ludovic Capelli

PROGRAM main
    USE util
    USE mpi

    IMPLICIT NONE
   
    !> Used to get the error code returned by MPI routines
    INTEGER :: ierr
    !> Ranks for convenience so that we don't throw raw values all over the code
    INTEGER, PARAMETER :: MASTER_PROCESS_RANK = 0
    !> The rank of the MPI process in charge of this instance
    INTEGER :: my_rank
    !> Number of MPI processes in total, commonly called 'comm_size' for 'communicator size'.
    INTEGER :: comm_size
    !> Rank of the first MPI process
    INTEGER, PARAMETER :: FIRST_PROCESS_RANK = 0
    !> Rank of the last MPI process
    INTEGER :: LAST_PROCESS_RANK
    !> Rank of my up neighbour if any
    INTEGER :: up_neighbour_rank
    !> Rank of my down neighbour if any
    INTEGER :: down_neighbour_rank
    !> Array that will contain my part chunk. It will include the 2 ghost rows (1 up, 1 down)
    REAL(8), DIMENSION(0:ROWS_PER_MPI_PROCESS+1,0:COLUMNS_PER_MPI_PROCESS-1) :: temperatures
    !> Temperatures from the previous iteration, same dimensions as the array above.
    REAL(8), DIMENSION(0:ROWS_PER_MPI_PROCESS+1,0:COLUMNS_PER_MPI_PROCESS-1) :: temperatures_last
    !> On master process only: contains all temperatures read from input file.
    REAL(8), DIMENSION(0:ROWS-1,0:COLUMNS-1) :: all_temperatures
    !> Will contain the entire time elapsed in the timed portion of the code
    REAL(8) :: total_time_so_far = 0.0
    !> Contains the timestamp as measured at the beginning of the timed portion of the code
    REAL(8) :: start_time
    !> Iterator
    INTEGER :: i
    !> Iterator
    INTEGER :: j
    !> Iterator
    INTEGER :: k
    !> Iterator
    INTEGER :: l
    !> Keep track of the current iteration count
    INTEGER :: iteration_count = 0
    !> Maximum temperature change observed across all MPI processes
    REAL(8) :: global_temperature_change
    !> Maximum temperature change for us
    REAL(8) :: my_temperature_change 
    !> Used to store temperatures changes from other MPI processes
    REAL(8) :: subtotal
    !> The last snapshot made
    REAL(8), DIMENSION(0:ROWS-1,0:COLUMNS-1) :: snapshot
  
    CALL MPI_Init(ierr)

    ! /////////////////////////////////////////////////////
    ! ! -- PREPARATION 1: COLLECT USEFUL INFORMATION -- //
    ! /////////////////////////////////////////////////////
    CALL MPI_Comm_rank(MPI_COMM_WORLD, my_rank, ierr)

    CALL MPI_Comm_size(MPI_COMM_WORLD, comm_size, ierr)
    LAST_PROCESS_RANK = comm_size - 1

    up_neighbour_rank = merge(MPI_PROC_NULL, my_rank - 1, my_rank .EQ. FIRST_PROCESS_RANK)
    
    down_neighbour_rank = merge(MPI_PROC_NULL, my_rank + 1, my_rank .EQ. LAST_PROCESS_RANK)

    ! ////////////////////////////////////////////////////////////////////
    ! ! -- PREPARATION 2: INITIALISE TEMPERATURES ON MASTER PROCESS -- //
    ! ////////////////////////////////////////////////////////////////////

    ! The master MPI process will read a chunk from the file, send it to the corresponding MPI process and repeat until all chunks are read.
    IF (my_rank == MASTER_PROCESS_RANK) THEN
        CALL initialise_temperatures(all_temperatures)
    END IF

    CALL MPI_Barrier(MPI_COMM_WORLD, ierr)

    ! ///////////////////////////////////////////
    ! !     ^                                 //
    ! !    / \                                //
    ! !   / | \    CODE FROM HERE IS TIMED    //
    ! !  /  o  \                              //
    ! ! /_______\                             //
    ! ///////////////////////////////////////////
    
    ! ////////////////////////////////////////////////////////
    ! ! -- TASK 1: DISTRIBUTE DATA TO ALL MPI PROCESSES -- //
    ! ////////////////////////////////////////////////////////
    start_time = MPI_Wtime()

    IF (my_rank .EQ. MASTER_PROCESS_RANK) THEN
        DO i = 0, comm_size-1
            ! Is the i'th chunk meant for me, the master MPI process?
            IF (i .NE. my_rank) THEN
                ! No, so send the corresponding chunk to that MPI process.
                CALL MPI_Ssend(all_temperatures(i * ROWS_PER_MPI_PROCESS,0), ROWS_PER_MPI_PROCESS * COLUMNS_PER_MPI_PROCESS, &
                               MPI_DOUBLE_PRECISION, i, 0, MPI_COMM_WORLD, ierr)
            ELSE
                ! Yes, let's copy it straight for the array in which we read the file into.
                DO j = 1, ROWS_PER_MPI_PROCESS
                    DO k = 0, COLUMNS_PER_MPI_PROCESS-1
                        temperatures_last(j,k) = all_temperatures(j-1,k)
                    ENDDO
                ENDDO
            END IF
        ENDDO
    ELSE
        ! Receive my chunk.
        CALL MPI_Recv(temperatures_last(1,0), ROWS_PER_MPI_PROCESS * COLUMNS_PER_MPI_PROCESS, MPI_DOUBLE_PRECISION, MASTER_PROCESS_RANK, &
                      MPI_ANY_TAG, MPI_COMM_WORLD, MPI_STATUS_IGNORE, ierr)
    END IF

    ! Copy the temperatures into the current iteration temperature as well
    DO i = 1, ROWS_PER_MPI_PROCESS
        DO j = 1, COLUMNS_PER_MPI_PROCESS - 1
            temperatures(i,j) = temperatures_last(i,j)
        ENDDO
    ENDDO

    IF (my_rank == MASTER_PROCESS_RANK) THEN
        WRITE(*,*) 'Data acquisition complete.'
    END IF

    ! Wait for everybody to receive their part before we can start processing
    CALL MPI_Barrier(MPI_COMM_WORLD, ierr)
    
    ! /////////////////////////////
    ! // TASK 2: DATA PROCESSING //
    ! /////////////////////////////

    DO WHILE (total_time_so_far .LT. MAX_TIME)
        my_temperature_change = 0.0

        ! ////////////////////////////////////////
        ! -- SUBTASK 1: EXCHANGE GHOST CELLS -- //
        ! ////////////////////////////////////////

        ! Send data to up neighbour for its ghost cells. If my up_neighbour_rank is MPI_PROC_NULL, this MPI_Ssend will do nothing.
        CALL MPI_Ssend(temperatures(1,0), COLUMNS_PER_MPI_PROCESS, MPI_DOUBLE_PRECISION, up_neighbour_rank, 0, MPI_COMM_WORLD, ierr)

        ! Receive data from down neighbour to fill our ghost cells. If my down_neighbour_rank is MPI_PROC_NULL, this MPI_Recv will do nothing.
        CALL MPI_Recv(temperatures_last(ROWS_PER_MPI_PROCESS+1,0), COLUMNS_PER_MPI_PROCESS, MPI_DOUBLE_PRECISION, down_neighbour_rank, &
                      MPI_ANY_TAG, MPI_COMM_WORLD, MPI_STATUS_IGNORE, ierr)

        ! Send data to down neighbour for its ghost cells. If my down_neighbour_rank is MPI_PROC_NULL, this MPI_Ssend will do nothing.
        CALL MPI_Ssend(temperatures(ROWS_PER_MPI_PROCESS,0), COLUMNS_PER_MPI_PROCESS, MPI_DOUBLE_PRECISION, down_neighbour_rank, 0, &
                       MPI_COMM_WORLD, ierr)

        ! Receive data from up neighbour to fill our ghost cells. If my up_neighbour_rank is MPI_PROC_NULL, this MPI_Recv will do nothing.
        CALL MPI_Recv(temperatures_last(0,0), COLUMNS_PER_MPI_PROCESS, MPI_DOUBLE_PRECISION, up_neighbour_rank, MPI_ANY_TAG, MPI_COMM_WORLD, &
                      MPI_STATUS_IGNORE, ierr)

        ! /////////////////////////////////////////////
        ! // -- SUBTASK 2: PROPAGATE TEMPERATURES -- //
        ! /////////////////////////////////////////////
        DO i = 1, ROWS_PER_MPI_PROCESS
            ! Process the cell at the first column, which has no left neighbour
            IF (temperatures(i,0) .NE. MAX_TEMPERATURE) THEN
                temperatures(i,0) = (temperatures_last(i-1,0) + &
                                     temperatures_last(i+1,0) + &
                                     temperatures_last(i  ,1)) / 3.0
            END IF
            ! Process all cells between the first and last columns excluded, which each has both left and right neighbours
            DO j = 1, COLUMNS_PER_MPI_PROCESS - 2
                IF (temperatures(i,j) .NE. MAX_TEMPERATURE) THEN
                    temperatures(i,j) = 0.25 * (temperatures_last(i-1,j  ) + &
                                                temperatures_last(i+1,j  ) + &
                                                temperatures_last(i  ,j-1) + &
                                                temperatures_last(i  ,j+1))
                END IF
            END DO
            ! Process the cell at the last column, which has no right neighbour
            IF (temperatures(i,COLUMNS_PER_MPI_PROCESS - 1) .NE. MAX_TEMPERATURE) THEN
                temperatures(i,COLUMNS_PER_MPI_PROCESS - 1) = (temperatures_last(i-1,COLUMNS_PER_MPI_PROCESS - 1) + &
                                                                temperatures_last(i+1,COLUMNS_PER_MPI_PROCESS - 1) + &
                                                                temperatures_last(i  ,COLUMNS_PER_MPI_PROCESS - 2)) / 3.0
            END IF
        END DO

        ! ///////////////////////////////////////////////////////
        ! // -- SUBTASK 3: CALCULATE MAX TEMPERATURE CHANGE -- //
        ! ///////////////////////////////////////////////////////
        my_temperature_change = 0.0
        DO i = 1, ROWS_PER_MPI_PROCESS
            DO j = 0, COLUMNS_PER_MPI_PROCESS-1
                my_temperature_change = max(abs(temperatures(i,j) - temperatures_last(i,j)), my_temperature_change)
            END DO
        END DO

        ! //////////////////////////////////////////////////////////
        ! // -- SUBTASK 4: FIND MAX TEMPERATURE CHANGE OVERALL -- //
        ! //////////////////////////////////////////////////////////
        IF (my_rank .NE. MASTER_PROCESS_RANK) THEN
            ! Send my temperature delta to the master MPI process
            CALL MPI_Ssend(my_temperature_change, 1, MPI_DOUBLE_PRECISION, MASTER_PROCESS_RANK, 0, MPI_COMM_WORLD, ierr)
            
            ! Receive the total delta calculated by the MPI process based on all MPI processes delta
            CALL MPI_Recv(global_temperature_change, 1, MPI_DOUBLE_PRECISION, MASTER_PROCESS_RANK, MPI_ANY_TAG, MPI_COMM_WORLD, &
                          MPI_STATUS_IGNORE, ierr)
        ELSE
            ! Initialise the temperature change to mine
            global_temperature_change = my_temperature_change

            ! Pick highest temperature change observed
            DO j = 0, comm_size-1
                IF (j .NE. my_rank) THEN
                    CALL MPI_Recv(subtotal, 1, MPI_DOUBLE_PRECISION, j, MPI_ANY_TAG, MPI_COMM_WORLD, MPI_STATUS_IGNORE, ierr)
                    IF (subtotal .GT. global_temperature_change) THEN
                        global_temperature_change = subtotal
                    END IF
                END IF
            END DO

            ! Send delta back to all MPI processes
            DO j = 0, comm_size-1
                IF (j .NE. my_rank) THEN
                    CALL MPI_Ssend(global_temperature_change, 1, MPI_DOUBLE_PRECISION, j, 0, MPI_COMM_WORLD, ierr)
                END IF
            END DO
        END IF

        ! //////////////////////////////////////////////////
        ! // -- SUBTASK 5: UPDATE LAST ITERATION ARRAY -- //
        ! //////////////////////////////////////////////////
        DO i = 1, ROWS_PER_MPI_PROCESS
            DO j = 0, COLUMNS_PER_MPI_PROCESS-1
                temperatures_last(i,j) = temperatures(i,j)
            END DO
        END DO

        ! ///////////////////////////////////
        ! // -- SUBTASK 6: GET SNAPSHOT -- //
        ! ///////////////////////////////////
        IF (MOD(iteration_count, SNAPSHOT_INTERVAL) .EQ. 0) THEN
            IF (my_rank == MASTER_PROCESS_RANK) THEN
                DO j = 0, comm_size-1
                    IF (j .EQ. my_rank) THEN
                        ! Copy locally my own temperature array in the global one
                        DO k = 0, ROWS_PER_MPI_PROCESS-1
                            DO l = 0, COLUMNS_PER_MPI_PROCESS-1
                                snapshot(j * ROWS_PER_MPI_PROCESS + k,l) = temperatures(k + 1,l)
                            END DO
                        END DO
                    ELSE
                        CALL MPI_Recv(snapshot(j * ROWS_PER_MPI_PROCESS,0), ROWS_PER_MPI_PROCESS * COLUMNS_PER_MPI_PROCESS, &
                                      MPI_DOUBLE_PRECISION, j, MPI_ANY_TAG, MPI_COMM_WORLD, MPI_STATUS_IGNORE, ierr)
                    END IF
                END DO

                WRITE(*,'(A,I0,A,F20.18)') 'Iteration ', iteration_count, ': ', global_temperature_change
            ELSE
                ! Send my array to the master MPI process
                CALL MPI_Ssend(temperatures(1,0), ROWS_PER_MPI_PROCESS * COLUMNS_PER_MPI_PROCESS, MPI_DOUBLE_PRECISION, MASTER_PROCESS_RANK, &
                               0, MPI_COMM_WORLD, ierr) 
            END IF
        END IF

        ! Calculate the total time spent processing
        IF (my_rank == MASTER_PROCESS_RANK) THEN
            total_time_so_far = MPI_Wtime() - start_time
        END IF

        ! Send total timer to everybody so they too can exit the loop if more than the allowed runtime has elapsed already
        CALL MPI_Bcast(total_time_so_far, 1, MPI_DOUBLE_PRECISION, MASTER_PROCESS_RANK, MPI_COMM_WORLD, ierr)

        ! Update the iteration number
        iteration_count = iteration_count + 1
    END DO

    ! ///////////////////////////////////////////////
    ! //     ^                                     //
    ! //    / \                                    //
    ! //   / | \    CODE FROM HERE IS NOT TIMED    //
    ! //  /  o  \                                  //
    ! // /_______\                                 //
    ! ///////////////////////////////////////////////

    ! /////////////////////////////////////////
    ! // -- FINALISATION 2: PRINT SUMMARY -- //
    ! /////////////////////////////////////////
    IF (my_rank == MASTER_PROCESS_RANK) THEN
        WRITE(*,'(A,F5.2,A,I0,A)') 'The program took ', total_time_so_far, ' seconds in total and executed ', iteration_count, &
                                   ' iterations.'
    END IF

    CALL MPI_Finalize(ierr)
END PROGRAM main
