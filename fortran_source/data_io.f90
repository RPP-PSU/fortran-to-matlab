! FT-IR Support Subroutines - Data I/O Operations
! Contains subroutines for reading and writing spectroscopic data

      SUBROUTINE READIFG(FNAME, X, Y, N, IERR)
      IMPLICIT NONE
      CHARACTER*80, INTENT(IN) :: FNAME
      INTEGER, INTENT(IN) :: N
      REAL*8, INTENT(OUT) :: X(N), Y(N)
      INTEGER, INTENT(OUT) :: IERR
      INTEGER :: I, IU
      
      IU = 10
      IERR = 0
      
      OPEN(UNIT=IU, FILE=FNAME, STATUS='OLD', IOSTAT=IERR)
      IF (IERR .NE. 0) RETURN
      
      DO I = 1, N
         READ(IU, *, IOSTAT=IERR) X(I), Y(I)
         IF (IERR .NE. 0) EXIT
      END DO
      
      CLOSE(IU)
      END SUBROUTINE READIFG
      
      
      SUBROUTINE WRITESP(FNAME, FREQ, SPEC, N)
      IMPLICIT NONE
      CHARACTER*80, INTENT(IN) :: FNAME
      INTEGER, INTENT(IN) :: N
      REAL*8, INTENT(IN) :: FREQ(N), SPEC(N)
      INTEGER :: I, IU
      
      IU = 20
      
      OPEN(UNIT=IU, FILE=FNAME, STATUS='REPLACE')
      
      DO I = 1, N
         WRITE(IU, '(2E16.8)') FREQ(I), SPEC(I)
      END DO
      
      CLOSE(IU)
      END SUBROUTINE WRITESP
      
      
      SUBROUTINE GENFREQ(FREQ, WN1, WN2, N)
      IMPLICIT NONE
      INTEGER, INTENT(IN) :: N
      REAL*8, INTENT(IN) :: WN1, WN2
      REAL*8, INTENT(OUT) :: FREQ(N)
      REAL*8 :: DWN
      INTEGER :: I
      
      DWN = (WN1 - WN2) / REAL(N-1)
      
      DO I = 1, N
         FREQ(I) = WN1 - (I-1) * DWN
      END DO
      
      END SUBROUTINE GENFREQ