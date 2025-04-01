PROGRAM MAIN
    USE PARAMETERS
    USE MD
    IMPLICIT NONE
    REAL, DIMENSION(N, D) :: R, V ! POSITION AND VELOCITY VECTORS
    INTEGER :: I

    CALL RANDOM_NUMBER(R)
    CALL RANDOM_NUMBER(V)

    R = R * 100
    V = (V * 200) - 100

    PRINT *, "POSITIONS:"
    PRINT *, R
    PRINT *, "VELOCITIES:"
    PRINT *, V
    
    DO I = 0, 100
        PRINT '(A, F10.6)', "POSITION AT T = ", DT * I
        PRINT *, R
        PRINT '(A, F10.6)', "VELOCITY AT T = ", DT * I
        PRINT *, V
        CALL UPDATE(R, V)
    END DO

    CONTAINS
        SUBROUTINE DUMP(R, T)
            CHARACTER(LEN=20) :: FNAME
            REAL, DIMENSION(N, D) :: R
            REAL :: T


        RETURN
        END

END PROGRAM MAIN