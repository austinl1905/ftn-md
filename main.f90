PROGRAM MAIN
    USE PARAMETERS
    USE MD
    IMPLICIT NONE
    REAL, DIMENSION(N, D) :: R, V, A ! POSITION AND VELOCITY VECTORS
    INTEGER :: I, J
    REAL :: TPOTE = 0

    CALL RANDOM_NUMBER(R)
    CALL RANDOM_NUMBER(V)

    R = R * L
    V = (V * 200) - 100
    
    DO I = 1, 50
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
        PRINT '(A, F10.6)', "TOTAL POTENTIAL ENERGY AT T = ", DT * I
        DO J = 1, N
            TPOTE = TPOTE + LJPOT(R, J)
        END DO
        PRINT *, TPOTE
        CALL DUMP(R, DT * I, I)
    END DO
    

END PROGRAM MAIN