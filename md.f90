MODULE MD
    USE PARAMETERS
    IMPLICIT NONE

    CONTAINS

    FUNCTION UPDATE(R, V) RESULT(NEWR)
        REAL, DIMENSION(N, D), INTENT(IN) :: R, V
        REAL, DIMENSION(N, D):: NEWR
        NEWR = R + V * DT
        IF (BC) THEN
            NEWR = MODULO(R + V * DT, L)
        ELSE 
            CALL REFLECT(R, V)
        END IF
        RETURN
    END FUNCTION

    SUBROUTINE REFLECT(R, V)
        REAL, DIMENSION(N, D) :: R, V
        INTEGER :: COL, ROW
            DO COL = 0, SIZE(R, DIM = 2)
                DO ROW = 0, SIZE(R, DIM = 1)
                    IF (R(ROW, COL).LT.0) THEN 
                        V(ROW,COL) = -V(ROW, COL)
                        R(ROW, COL) = -R(ROW, COL)
                    END IF
                    IF (R(ROW, COL).GT.100) THEN
                        V(ROW, COL) = -V(ROW, COL)
                        R(ROW, COL) = (2.0 * 100) - R(ROW, COL)
                    END IF
                END DO
            END DO
        RETURN
    END
END MODULE