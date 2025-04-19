PROGRAM MAIN
    USE PARAMETERS
    USE MD
    IMPLICIT NONE
    REAL, DIMENSION(N, D) :: R, V, A ! POSITION, VELOCITY, AND ACCELERATION VECTORS
    INTEGER :: I, J, START, FINISH, RATE
    REAL :: TIME_ELAPSED

    CALL SYSTEM_CLOCK(COUNT_RATE=RATE)
    CALL SYSTEM_CLOCK(START)

    ! CALL RANDOM_NUMBER(R)
    ! CALL RANDOM_NUMBER(V)

    R = R * L
    V = (V * 100) - 50

    DO I = 1, N ! INITIALIZE ACCELERATION
        A(I, :) = DLJPOT(R, I) / M
    END DO
    
    DO I = 1, 1000
        CALL VEL_VERLET(R, V, A)
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

    CALL SYSTEM_CLOCK(FINISH)
    TIME_ELAPSED = REAL(FINISH - START)/ REAL(RATE)

    PRINT *, 'ELAPSED TIME: ', TIME_ELAPSED, ' S'

END PROGRAM MAIN