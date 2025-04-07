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
        INTEGER :: I, J, K

        WRITE(FILENAME, '(A, I0, A)') 'dump/data', I, '.txt'

        OPEN(1, FILE=FILENAME, STATUS = 'new')

        WRITE(1, *) "ITEM: STEP"
        WRITE(1, '(I10)') I
        WRITE(1, *) "ITEM: DT"
        WRITE(1, '(F5.2)') DT
        WRITE(1, *) "ITEM: TIME"
        WRITE(1, '(F5.2)') T
        WRITE(1, *) "ITEM: N ATOMS"
        WRITE(1, '(I10)') N
        WRITE(1, *) "ITEM: BOX CONDITION"

        IF (BC.EQ.1) THEN 
            WRITE(1, *) "PERIODIC"
        ELSE IF (BC.EQ.0) THEN
            WRITE(1, *) "REFLECTIVE"
        END IF

        WRITE(1, *) "ITEM: BOX BOUNDS (CUBE)"
        WRITE(1, '(F5.3, A, F7.3)') 0.00, " ", L

        WRITE(1, *) "ITEM: X Y Z"
        
        DO J = 1, N
            WRITE(1, '(F10.6, F10.6, F10.6)') R(J, 1), R(J, 2), R(J, 3)
        END DO

        CLOSE(1)
    END

    FUNCTION LJPOT(R, A) RESULT(LJP) ! SUM OF PAIRWISE POTENTIAL ENERGIES BETWEEN MOLECULE A AND EVERY OTHER MOLECULE IN SIMULATION. FOR THE TOTAL POTENTIAL ENERY OF THE SYSTEM, IT IS REQUIRED TO SUM THE FUNCTION CALL FOR EVERY MOLECULE IN THIS SYSTEM 
        REAL, DIMENSION(N, D) :: R, DRV ! POSITION, POTENTIAL ENERGY, DISPLACEMENT VECTOR (FOR EACH DIMENSION AND ATOM)
        REAL, DIMENSION(N - 1, D) :: NDRV ! DISPLACEMENT VECTOR (WITHOUT ITH ATOM)
        REAL, DIMENSION(N - 1) :: DR, LJPS ! DISTANCE BETWEEN NTH ATOM AND ITH ATOM
        REAL :: LJP
        INTEGER :: A, NEW_ROW, I

        NEW_ROW = 1

        DO I = 1, N ! CALCULATE COMPONENT WISE DISTANCE VECTORS. EX: IF ATOM K HAS POSITION VECTOR (1, 2, 3) AND ATOM L HAS POSITION VECTOR (5, 4, 2) THEN THE RESULTING VECTOR IS (4, 2, -1). THIS VECTOR CAN THEN BE USED FOR EUCLIDEAN DISTANCE CALCULATION.
            DRV(I, :) = R(I, :) - R(A, :)
        END DO

        DO I = 1, N 
            IF (I.EQ.A) THEN ! EXCLUDE ITH ATOM
                CYCLE
            END IF
            NDRV(NEW_ROW, :) = DRV(I, :)
            NEW_ROW = NEW_ROW + 1
        END DO

        DO I = 1, N - 1 ! CALCULATE EUCLIDEAN DISTANCE FOR EACH ATOM (ASSUMING D = 3)
            DR(I) = SQRT(NDRV(I, 1)**2 + NDRV(I, 2)**2 + NDRV(I, 3)**2)
        END DO

        DO I = N, N - 1 ! CALCULATE LENNARD-JONES POTENTIAL
            LJPS(I) = 4.00 * EPS * (((SIG/DR(I))**12)-((SIG/DR(I))**6))
        END DO

        LJP = SUM(LJPS)

    RETURN
    END
END MODULE