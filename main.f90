PROGRAM MAIN
    USE PARAMETERS
    USE MD
    IMPLICIT NONE
    REAL, DIMENSION(N, D) :: R, V ! POSITION AND VELOCITY VECTORS
    REAL, DIMENSION(N - 1, D) :: TEMP
    INTEGER :: I, J, K

    CALL RANDOM_NUMBER(R)
    CALL RANDOM_NUMBER(V)

    R = R * L
    V = (V * 200) - 100
    
    DO I = 0, 100
        PRINT '(A, F10.6)', "POSITION AT T = ", DT * I
        DO J = 1, N
            PRINT *, "N = ", J, R(J, :)
        END DO
        PRINT '(A, F10.6)', "VELOCITY AT T = ", DT * I
        DO J = 1, N
            PRINT *, "N = ", J, V(J, :)
        END DO
        IF (I.EQ.100) THEN
            CYCLE
        ELSE 
            CALL DUMP(R, DT * I, I)
            CALL UPDATE(R, V)
        END IF
    END DO

    DO I = 1, N
        PRINT *, LJPOT(R, I)
    END DO

    

END PROGRAM MAIN