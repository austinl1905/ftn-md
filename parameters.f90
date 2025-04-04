MODULE PARAMETERS
    IMPLICIT NONE
    INTEGER, PARAMETER :: N = 5, D = 3, BC = 0 ! NUMBER OF MOLECULES, DIMENSIONS, BOUNDARY CONDITIONS
    REAL, PARAMETER :: DT = 0.01, L = 100.000, EPS = 1.0, SIG=1.0, M=1.0 ! TIME STEP, BOUNDS, LJ PARAMS, MASS
END MODULE PARAMETERS