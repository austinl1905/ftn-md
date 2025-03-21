MODULE MD
    USE PARAMETERS
    IMPLICIT NONE
    PUBLIC :: VECTORS
    TYPE VECTORS
        REAL, PUBLIC, DIMENSION(N, D) :: POS
        REAL, PUBLIC, DIMENSION(N, D) :: VEL
    END TYPE VECTORS

    CONTAINS

    FUNCTION UPDATE(R, V, DT) RESULT(NEWR)
        REAL, DIMENSION(N, D), INTENT(IN) :: R, V
        REAL, INTENT(IN) :: DT
        REAL, DIMENSION(N, D):: NEWR
        NEWR = R + V * DT
        IF (BC) THEN
            NEWR = MODULO(R + V * DT, L)
        END IF
        RETURN
    END FUNCTION

    VECTORS FUNCTION REFLECT(R, V, DT) RESULT(RES)
        REAL, DIMENSION(N, D), INTENT(IN) :: R, V
        REAL, INTENT(IN) :: DT
        REAL, DIMENSION(N, D):: NEWR, NEWV
        CLASS(VECTORS) :: RES

        RES%POS = NEWR
        RES%VEL = NEWV
        
    END FUNCTION
END MODULE MD