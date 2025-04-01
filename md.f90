MODULE MD
    USE PARAMETERS
    IMPLICIT NONE

    CONTAINS

    SUBROUTINE UPDATE(R, V)
        REAL, DIMENSION(N, D) :: R, V
        REAL, DIMENSION(N, D):: NEWR
        R = R + V * DT
        IF (BC) THEN ! PERIODIC BOUNDARY CONDITIONS
            R = MODULO(R + V * DT, L)
        ELSE
            CALL REFLECT(R, V) ! USE REFLECTING BOUNDARY CONDITIONS
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
END MODULE