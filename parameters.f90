MODULE PARAMETERS
    IMPLICIT NONE
    INTEGER, PARAMETER :: N = 200, D = 3, BC = 1 ! NUMBER OF MOLECULES, DIMENSIONS, BOUNDARY CONDITIONS
    REAL, PARAMETER :: DT = 0.001, L = 100.000, EPS = 4.4778900, SIG=0.5523570, M=1.00784 ! TIME STEP, BOUNDS, LJ PARAMS, MASS
END MODULE PARAMETERS