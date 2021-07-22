MODULE util
    IMPLICIT NONE

    #IFDEF BIG
        REAL(8), PARAMETER :: MAX_TIME = 30.0
    #ELSE  
        REAL(8), PARAMETER :: MAX_TIME = 1.0
    #ENDIF
    INTEGER, PARAMETER :: SNAPSHOT_INTERVAL = 50

CONTAINS

    SUBROUTINE initialise_temperatures(temperatures)
        IMPLICIT NONE

        REAL(8), DIMENSION(0:ROWS+1, 0:COLUMNS-1) :: temperatures
        INTEGER, PARAMETER :: MID_ROWS = ROWS / 2
        INTEGER, PARAMETER :: MID_COLUMNS = COLUMNS / 2
        INTEGER, PARAMETER :: THICKNESS = ROWS / 2
        INTEGER :: i
        INTEGER :: j
        #ifdef BIG
            DO i = 0, ROWS - 1
                DO j = 0, COLUMNS -1
                    IF (i .GE. (MID_ROWS - THICKNESS/2) .AND. i .LE. (MID_ROWS + THICKNESS/2) .AND. &
                        j .GE. (MID_COLUMNS - THICKNESS/2) .AND. j .LE. (MID_COLUMNS + THICKNESS/2)) THEN
                        temperatures(i,j) = MAX_TEMPERATURE
                    ELSE
                        temperatures(i,j) = 0.0;
                    END IF
                END DO
           END DO
        #else
            DO i = 0, ROWS-1
                DO j = 0, COLUMNS-1
                    temperatures(i,j) = merge(MAX_TEMPERATURE, 0.0, MOD(j, 100) .EQ. 0)
                END DO
            END DO
        #endif
    END SUBROUTINE
END MODULE
