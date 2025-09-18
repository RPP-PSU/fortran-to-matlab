! FT-IR Support Subroutines - Signal Processing Operations
! Contains FFT, apodization, and phase correction routines

      SUBROUTINE APOD(X, Y, N, APTYPE)
      IMPLICIT NONE
      INTEGER, INTENT(IN) :: N
      REAL*8, INTENT(INOUT) :: X(N), Y(N)
      CHARACTER*4, INTENT(IN) :: APTYPE
      REAL*8 :: W
      INTEGER :: I
      REAL*8, PARAMETER :: PI = 3.14159265358979D0
      
      SELECT CASE(APTYPE)
      CASE('HAPP')  ! Happ-Genzel apodization
         DO I = 1, N
            W = 0.54D0 + 0.46D0 * COS(PI * (I-1) / (N-1))
            Y(I) = Y(I) * W
         END DO
      CASE('HANN')  ! Hanning apodization  
         DO I = 1, N
            W = 0.5D0 * (1.0D0 + COS(PI * (I-1) / (N-1)))
            Y(I) = Y(I) * W
         END DO
      CASE('HAMM')  ! Hamming apodization
         DO I = 1, N
            W = 0.54D0 - 0.46D0 * COS(2.0D0 * PI * (I-1) / (N-1))
            Y(I) = Y(I) * W
         END DO
      END SELECT
      
      END SUBROUTINE APOD
      
      
      SUBROUTINE FFT(X, Y, Z, N)
      IMPLICIT NONE
      INTEGER, INTENT(IN) :: N
      REAL*8, INTENT(IN) :: X(N)
      REAL*8, INTENT(INOUT) :: Y(N)
      REAL*8, INTENT(OUT) :: Z(N)
      INTEGER :: I, J, K, M
      REAL*8 :: TEMP, WR, WI, TR, TI
      REAL*8, PARAMETER :: PI = 3.14159265358979D0
      
      ! Bit reversal
      J = 1
      DO I = 1, N
         IF (I .LT. J) THEN
            TEMP = Y(I)
            Y(I) = Y(J)
            Y(J) = TEMP
         END IF
         K = N / 2
         DO WHILE (K .GE. 1 .AND. J .GT. K)
            J = J - K
            K = K / 2
         END DO
         J = J + K
      END DO
      
      ! Initialize imaginary part
      DO I = 1, N
         Z(I) = 0.0D0
      END DO
      
      ! FFT computation
      M = 1
      DO WHILE (M .LT. N)
         M = M * 2
         DO I = 1, N, M
            DO J = I, I + M/2 - 1
               K = J + M/2
               WR = COS(-2.0D0 * PI * (J-I) / M)
               WI = SIN(-2.0D0 * PI * (J-I) / M)
               TR = WR * Y(K) - WI * Z(K)
               TI = WR * Z(K) + WI * Y(K)
               Y(K) = Y(J) - TR
               Z(K) = Z(J) - TI
               Y(J) = Y(J) + TR
               Z(J) = Z(J) + TI
            END DO
         END DO
      END DO
      
      END SUBROUTINE FFT
      
      
      SUBROUTINE PHCORR(Y, Z, PHASE, N)
      IMPLICIT NONE
      INTEGER, INTENT(IN) :: N
      REAL*8, INTENT(INOUT) :: Y(N), Z(N)
      REAL*8, INTENT(IN) :: PHASE
      REAL*8 :: CP, SP, TR, TI
      INTEGER :: I
      
      CP = COS(PHASE)
      SP = SIN(PHASE)
      
      DO I = 1, N
         TR = Y(I) * CP - Z(I) * SP
         TI = Y(I) * SP + Z(I) * CP
         Y(I) = TR
         Z(I) = TI
      END DO
      
      END SUBROUTINE PHCORR
      
      
      SUBROUTINE MAKSPEC(Y, Z, SPEC, N)
      IMPLICIT NONE
      INTEGER, INTENT(IN) :: N
      REAL*8, INTENT(IN) :: Y(N), Z(N)
      REAL*8, INTENT(OUT) :: SPEC(N)
      INTEGER :: I
      
      DO I = 1, N
         SPEC(I) = SQRT(Y(I)**2 + Z(I)**2)
      END DO
      
      END SUBROUTINE MAKSPEC