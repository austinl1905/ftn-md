MODULE MD
    USE PARAMETERS
    IMPLICIT NONE

    CONTAINS

    SUBROUTINE UPDATE(R, V)
        REAL, DIMENSION(N, D) :: R, V
        R = R + V * DT
        IF (BC.EQ.1) THEN
            R = MODULO(R + V * DT, L)
        ELSE 
            CALL REFLECT(R, V)
        END IF
        RETURN
    END

    SUBROUTINE UPDATEV(R, V, A)
        REAL, DIMENSION(N, D) :: R, V, F, A
        INTEGER :: I

        DO I = 1, N
            F(I, :) = -DLJPOT(R, I)
        END DO

        A = F / M

        V = V + A * DT
    END

    SUBROUTINE REFLECT(R, V)
        REAL, DIMENSION(N, D) :: R, V
        INTEGER :: COL, ROW
            DO COL = 0, SIZE(R, DIM = 2)
                DO ROW = 0, SIZE(R, DIM = 1)
                    IF (R(ROW, COL).LT.0) THEN 
                        V(ROW,COL) = -V(ROW, COL)
                        R(ROW, COL) = -R(ROW, COL) ! REFLECT POSITION ABOUT 0
                    END IF
                    IF (R(ROW, COL).GT.L) THEN
                        V(ROW, COL) = -V(ROW, COL)
                        R(ROW, COL) = (2.0 * L) - R(ROW, COL) ! REFLECT POSITION ABOUT L
                    END IF
                END DO
            END DO
        RETURN
    END

    SUBROUTINE DUMP(R, T, I)
        CHARACTER(LEN = 20) :: FILENAME
        REAL, DIMENSION(N, D) :: R
        REAL :: T
        INTEGER :: I, J

        WRITE(FILENAME, '(A, I0, A)') 'dump/data', I, '.xyz'

        OPEN(1, FILE=FILENAME, STATUS = 'REPLACE')

        WRITE(1, '(I0)') N

        WRITE(1, '(A, F8.3, A, I5)') 'TIME: ', T, ', STEP: ', I
        
        DO J = 1, N
            WRITE(1, '(A1, 3F12.6)') 'H', R(J,1), R(J,2), R(J,3)
        END DO

        CLOSE(1)
    END

    FUNCTION LJPOT(R, A) RESULT(LJP) ! SUM OF PAIRWISE POTENTIAL ENERGIES BETWEEN MOLECULE A AND EVERY OTHER MOLECULE IN SIMULATION
        REAL, DIMENSION(N, D) :: R ! POSITIONS
        REAL, DIMENSION(N - 1, D) :: NR, DRV ! POSITION VECTOR (WITHOUT ITH ATOM), DISPLACEMENT
        REAL, DIMENSION(N - 1) :: DR, LJPS ! DISTANCE BETWEEN NTH ATOM AND ITH ATOM
        REAL, DIMENSION(3) :: AR
        REAL :: LJP
        INTEGER :: A, NEW_ROW, I

        NEW_ROW = 1

        DO I = 1, N 
            IF (I.EQ.A) THEN ! EXCLUDE ITH ATOM
                AR = R(I, :)
                CYCLE
            END IF
            NR(NEW_ROW, :) = R(I, :)
            NEW_ROW = NEW_ROW + 1
        END DO

        DO I = 1, N - 1 ! CALCULATE COMPONENT WISE DISTANCE VECTORS. EX: IF ATOM K HAS POSITION VECTOR (1, 2, 3) AND ATOM L HAS POSITION VECTOR (5, 4, 2) THEN THE RESULTING VECTOR IS (4, 2, -1). THIS VECTOR CAN THEN BE USED FOR EUCLIDEAN DISTANCE CALCULATION.
            DRV(I, :) = AR - NR(I, :)
        END DO

        DO I = 1, N - 1 ! CALCULATE EUCLIDEAN DISTANCE FOR EACH ATOM (ASSUMING D = 3)
            DR(I) = SQRT(SUM(DRV(I, :)**2))
        END DO

        LJPS = 4.00 * EPS * ((SIG/DR)**12-(SIG/DR)**6)

        LJP = SUM(LJPS) ! POTENTIAL ENERGY IS A SCALAR QUANTITY

    RETURN
    END

    FUNCTION DLJPOT(R, A) RESULT (DLJPS)
        REAL, DIMENSION(N, D) :: R ! POSITIONS
        REAL, DIMENSION(N - 1, D) :: NR, DRV, UR, DLJP ! POSITIONS (EXCLUDING A), DISPLACEMENT (A-I), UNIT VECTORS, POTENTIAL GRADIENT
        REAL, DIMENSION(N - 1) :: DR, DUDR ! DISTANCE (MAGNITUDE OF DRV), DERIVATIVE OF ENERGY WITH RESPECT TO DISTANCE
        REAL, DIMENSION(3) :: AR, DLJPS ! POSITION OF A, SUM OF GRADIENTS
        INTEGER :: A, NEW_ROW, I

        NEW_ROW = 1

        DO I = 1, N 
            IF (I.EQ.A) THEN
                AR = R(I, :)
                CYCLE
            END IF
            NR(NEW_ROW, :) = R(I, :)
            NEW_ROW = NEW_ROW + 1
        END DO

        DO I = 1, N - 1
            DRV(I, :) = AR - NR(I, :)
        END DO

        DO I = 1, N - 1 !
            DR(I) = SQRT(SUM(DRV(I, :)**2))
        END DO

        DUDR = 24 * EPS * (2*(SIG**12/DR**13)-(SIG**6/DR**7)) ! CALCULATE DERIVATIVES

        DO I = 1, N - 1 ! CALCULATE UNIT VECTORS
            UR(I, :) = DRV(I, :) / DR(I)
        END DO
        
        DO I = 1, N - 1
            DLJP(I, :) = UR(I, :) * DUDR(I)
        END DO

        DLJPS = SUM(DLJP, DIM=1)

    RETURN
    END

END MODULE