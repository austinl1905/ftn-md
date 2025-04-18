PROGRAM MAIN
    USE PARAMETERS
    USE MD
    IMPLICIT NONE
    REAL, DIMENSION(N, D) :: R, V, A ! POSITION AND VELOCITY VECTORS
    INTEGER :: I, J

    CALL RANDOM_NUMBER(R)
    CALL RANDOM_NUMBER(V)

    R = R * L
    V = (V * 100) - 50
    
    DO I = 1, 1000
        CALL UPDATEV(R, V, A)
        CALL UPDATE(R, V)
        PRINT '(A, F10.6)', "POSITION AT T = ", DT * I
        DO J = 1, N
            PRINT *, "N = ", J, R(J, :)
        END DO
        PRINT '(A, F10.6)', "VELOCITY AT T = ", DT * I
        DO J = 1, N
            PRINT *, "N = ", J, V(J, :)
        END DO
        PRINT '(A, F10.6)', "ACCELERATION AT T = ", DT * I
        DO J = 1, N
            PRINT *, "N = ", J, A(J, :)
        END DO
        CALL DUMP(R, V, A, DT * I, I)
    END DO

END PROGRAM MAIN