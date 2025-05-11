MODULE MD
    USE PARAMETERS
    IMPLICIT NONE

    CONTAINS

    SUBROUTINE UPDATE(R, V) ! I DONT USE THIS
        REAL(KIND=8), DIMENSION(N, D) :: R, V
        R = R + V * DT ! EULER METHOD (THIS IS WHAT HAPPENS WHEN A HIGH SCHOOLER WITH VERY LIMITED KNOWLEDGE OF CALC TRIES TO DO MD)
        IF (BC.EQ.1) THEN
            R = MODULO(R + V * DT, L)
        ELSE 
            CALL REFLECT(R, V)
        END IF
        RETURN
    END

    SUBROUTINE UPDATE_V(R, V, A, M) ! I DONT USE THIS EITHER. ITS STILL HERE FOR HISTORY
        REAL(KIND=8), DIMENSION(N, D) :: R, V, F, A
        REAL(KIND=8), DIMENSION(N) :: M
        INTEGER :: I

        DO I = 1, N
            F(I, :) = DLJPOT(R, I, M)
        END DO

        DO I = 1, N
            A(I, :) = F(I, :) / M(I)
        END DO

        V = V + A * DT
        RETURN
    END

    SUBROUTINE VEL_VERLET(R, V, A, M)
        REAL(KIND=8), DIMENSION(N, D) :: R, V, F, A, AN
        REAL(KIND=8), DIMENSION(N) :: M
        INTEGER :: I

        R = R + (V * DT) + (0.5 * A * DT**2) ! UPDATE POSITION

        IF (BC.EQ.1) THEN
            R = MODULO(R, L)
        ELSE 
            CALL REFLECT(R, V)
        END IF

        DO I = 1, N
            F(I, :) = DLJPOT(R, I, M) ! FORCE IS SUPPOSED TO BE THE NEGATIVE OF THE GRADIENT. BUT I ALREADY ACCOUNTED FOR THAT WHEN CALCULATING THE DERIVATIVE SO THIS IS FINE.
        END DO

        DO I = 1, N
            AN(I, :) = F(I, :) / M(I)
        END DO

        ! AN = F / M

        V = V + (0.5 * (A + AN) * DT) ! UPDATE VELOCITY

        CALL ANDERSEN_THERMOSTAT(V, M) ! THERMOSTAT DOES NOT INFLUENCE FORCE CALCULATIONS

        A = AN

        RETURN
    END

    SUBROUTINE ANDERSEN_THERMOSTAT(V, M)
        REAL(KIND=8), DIMENSION(N, D) :: V
        REAL(KIND=8), DIMENSION(N) :: M
        REAL(KIND=8) :: STDEV, P_COLL, RAND
        INTEGER :: I, J

        P_COLL = 0.5

        DO I = 1, N
            CALL RANDOM_NUMBER(RAND)
            IF (RAND.LT.(P_COLL*DT)) THEN
                STDEV = SQRT(KB*T/M(I))
                DO J = 1, D
                    V(I, J) = GAUSSIAN() * STDEV
                END DO
            END IF
        END DO

        RETURN
    END

    REAL(KIND=8) FUNCTION GAUSSIAN() ! USING BOX-MULLER TRANSFORM
            REAL(8) :: R1, R2
            R1 = RAND()
            R2 = RAND()
            GAUSSIAN = SQRT(-2.0 * LOG(R1)) * COS(2.0 * PI * R2)
        RETURN
    END FUNCTION GAUSSIAN

    ! SIMPLE VELOCITY RESCALING. WILL FIGURE OUT A BETTER THERMOSTAT LATER
    SUBROUTINE RESCALE_V(V, M)
        REAL(KIND=8) :: LAMB, CURRENT_T
        REAL(KIND=8), DIMENSION(N) :: M
        REAL(KIND=8), DIMENSION(N, D):: V

        CURRENT_T = GET_T(V, M)
        LAMB = SQRT(T/CURRENT_T)
        V = V * LAMB
        RETURN
    END

    REAL(KIND=8) FUNCTION GET_T(V, M) ! HELPER FUNCTION BECAUSE I WANT T IN MY DUMP FILES TOO
        REAL(KIND=8) :: KE, AVKE, LAMB
        REAL(KIND=8), DIMENSION(N, D) :: V
        REAL(KIND=8), DIMENSION(N) :: M
        INTEGER :: I
        
        KE = 0

        ! IM SORRY BUT I CAN NEVER GET USED TO FORTRAN ARRAY OPERATIONS
        DO I = 1, N
            KE = KE + SUM(V(I,:)**2) * M(I)
        END DO

        KE = KE * 0.5
        AVKE = KE / N
        GET_T = (2.0/3.0) * AVKE/KB
        RETURN
    END

    SUBROUTINE REFLECT(R, V)
        REAL(KIND=8), DIMENSION(N, D) :: R, V
        INTEGER :: COL, ROW
            DO COL = 1, SIZE(R, DIM = 2)
                DO ROW = 1, SIZE(R, DIM = 1)
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

    SUBROUTINE DUMP(MOL_NAMES, R, V, A, PE, CURRENT_T, T, I)
        CHARACTER(LEN = 20) :: FILENAME
        REAL(KIND=8), DIMENSION(N, D) :: R, V, A
        CHARACTER(LEN=2), DIMENSION(N) :: MOL_NAMES
        REAL(KIND=8) :: T, PE, CURRENT_T
        INTEGER :: I, J

        WRITE(FILENAME, '(A, I0, A)') 'dump/data', I, '.xyz'

        OPEN(1, FILE=FILENAME, STATUS = 'REPLACE')

        WRITE(1, '(I0)') N

        WRITE(1, '(A, F8.3, A, I5)') 'TIME: ', T, ', STEP: ', I
        
        DO J = 1, N
            WRITE(1, '(A2, 3F12.6)') MOL_NAMES(J), R(J,1), R(J,2), R(J,3)
        END DO

        CLOSE(1)

        WRITE(FILENAME, '(A, I0, A)') 'dump/vel', I, '.txt'

        OPEN(2, FILE = FILENAME, STATUS = 'REPLACE')

        WRITE(2, '(I0)') N

        WRITE(2, '(A, F8.3, A, I5)') 'TIME: ', T, ', STEP: ', I

        DO J = 1, N
            WRITE(2, '(A2, 6F12.6)') MOL_NAMES(J), V(J,1), V(J,2), V(J,3)
        END DO

        WRITE(FILENAME, '(A, I0, A)') 'dump/acc', I, '.txt'

        OPEN(3, FILE = FILENAME, STATUS = 'REPLACE')
        WRITE(3, '(I0)') N

        WRITE(3, '(A, F8.3, A, I5)') 'TIME: ', T, ', STEP: ', I

        DO J = 1, N
            WRITE(3, '(A2, 6F12.6)') MOL_NAMES(J), A(J,1), A(J,2), A(J,3)
        END DO

        WRITE(FILENAME, '(A, I0, A)') 'dump/pe', I, '.txt'

        OPEN(4, FILE = FILENAME, STATUS = 'REPLACE')
        WRITE(4, '(I0)') N
        WRITE(4, '(A, F8.3, A, I5)') 'TIME: ', T, ', STEP: ', I
        WRITE(4, '(6F12.6)') PE

        WRITE(FILENAME, '(A, I0, A)') 'dump/temp', I, '.txt'

        OPEN(5, FILE = FILENAME, STATUS = 'REPLACE')
        WRITE(5, '(I0)') N
        WRITE(5, '(A, F8.3, A, I5)') 'TIME: ', T, ', STEP: ', I
        WRITE(5, '(6F12.6)') CURRENT_T
    END

    REAL(KIND=8) FUNCTION LJPOT(R, A) ! SUM OF PAIRWISE POTENTIAL ENERGIES BETWEEN MOLECULE A AND EVERY OTHER MOLECULE IN SIMULATION
        REAL(KIND=8), DIMENSION(N, D) :: R ! POSITIONS
        REAL(KIND=8), DIMENSION(N - 1, D) :: NR, DRV ! POSITION VECTOR (WITHOUT ITH ATOM), DISPLACEMENT
        REAL(KIND=8), DIMENSION(N - 1) :: DR, LJPS ! DISTANCE BETWEEN NTH ATOM AND ITH ATOM
        REAL(KIND=8), DIMENSION(3) :: AR
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
            ! MIN IMAGE CONVENTION
            DRV(I, :) = DRV(I, :) - L * NINT(DRV(I, :) / L)
        END DO

        DO I = 1, N - 1 ! CALCULATE EUCLIDEAN DISTANCE FOR EACH ATOM (ASSUMING D = 3)
            DR(I) = SQRT(SUM(DRV(I, :)**2))
        END DO

        LJPS = 4.00 * EPS * ((SIG/DR)**12-(SIG/DR)**6)

        LJPOT = SUM(LJPS) ! POTENTIAL ENERGY IS A SCALAR QUANTITY

    RETURN
    END

    FUNCTION DLJPOT(R, A, M) RESULT (DLJPS)
        REAL(KIND=8), DIMENSION(N, D) :: R ! POSITIONS
        REAL(KIND=8), DIMENSION(N) :: M
        REAL(KIND=8), DIMENSION(N - 1, D) :: NR, DRV, UR, DLJP ! POSITIONS (EXCLUDING A), DISPLACEMENT (A-I), UNIT VECTORS, POTENTIAL GRADIENT
        REAL(KIND=8), DIMENSION(N - 1) :: DR, DUDR ! DISTANCE (MAGNITUDE OF DRV), DERIVATIVE OF ENERGY WITH RESPECT TO DISTANCE
        REAL(KIND=8), DIMENSION(3) :: AR, DLJPS ! POSITION OF A, SUM OF GRADIENTS
        INTEGER :: A, NEW_ROW, I
        REAL(KIND=8) :: RC, DUDRM

        ! DUDRM = 16000.0 * SUM(M) / SIZE(M)
        DUDRM = 2**20
        RC = 2.5

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
            IF (BC.EQ.1) THEN
                DRV(I, :) = DRV(I, :) - L * NINT(DRV(I, :) / L)
            END IF
        END DO

        DO I = 1, N - 1 !
            DR(I) = SQRT(SUM(DRV(I, :)**2))
        END DO

        DO I = 1, N - 1
            IF (DR(I).LE.2.5) THEN ! SHIFTED FORCE
                DUDR(I) = 24 * EPS * ((2*(SIG**12/DR(I)**13)-(SIG**6/DR(I)**7))-(2*(SIG**12/RC**13)-(SIG**6/RC**7)))
            ELSE
                DUDR(I) = 0 ! FORCE IS EFFECTIVELY ZERO AT LARGE DISTANCES
            END IF
        END DO

        ! DUDR = 24 * EPS * (2*(SIG**12/DR**13)-(SIG**6/DR**7)) ! CALCULATE DERIVATIVES
        DUDR = MIN(DUDR, DUDRM) ! AVOID PHYSICALLY UNREASONABLE ACCELERATION

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