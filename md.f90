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

    FUNCTION LJPOT(R, I) RESULT(LJP)
        REAL, DIMENSION(N, D) :: R, DR ! POSITION, POTENTIAL ENERGY, DISTANCE VECTOR (FOR EACH DIMENSION AND ATOM)
        REAL, DIMENSION(N - 1, D) :: MDR, LJP
        INTEGER :: ROW, NEW_ROW, I

        NEW_ROW = 1

        DO ROW = 1, N ! CALCULATE COMPONENT WISE DISTANCE VECTORS
            DR(ROW, :) = R(ROW, :) - R(1, :)
        END DO

        DO ROW = 1, N 
            IF (ROW.EQ.I) THEN ! DON'T CALCULATE POTENTIAL FOR ITH ATOM
                CYCLE
            END IF
            MDR(NEW_ROW, :) = DR(ROW, :)
            NEW_ROW = NEW_ROW + 1
        END DO

        LJP = MDR

    RETURN
    END
END MODULE