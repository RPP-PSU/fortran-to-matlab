! FT-IR Spectroscopy Data Processing - Main Driver Program
! Original FORTRAN code for demonstration
! Processes infrared spectroscopy data using Fourier transform techniques

      PROGRAM FTIR_MAIN
      IMPLICIT NONE
      
      INTEGER, PARAMETER :: NPTS = 8192    ! Number of data points
      INTEGER, PARAMETER :: NSPEC = 100    ! Number of spectra
      REAL*8 :: X(NPTS), Y(NPTS), Z(NPTS)
      REAL*8 :: FREQ(NPTS), SPEC(NPTS)
      REAL*8 :: WN1, WN2, RES, PHASE
      INTEGER :: I, J, N, IERR
      CHARACTER*80 :: FNAME
      
      ! Initialize parameters
      WN1 = 4000.0D0    ! Starting wavenumber (cm-1)
      WN2 = 400.0D0     ! Ending wavenumber (cm-1)  
      RES = 4.0D0       ! Resolution (cm-1)
      PHASE = 0.0D0     ! Phase correction
      N = NPTS
      
      WRITE(*,*) 'FT-IR Data Processing Program'
      WRITE(*,*) 'Processing ', NSPEC, ' spectra'
      
      ! Generate frequency axis
      CALL GENFREQ(FREQ, WN1, WN2, N)
      
      ! Main processing loop
      DO I = 1, NSPEC
         ! Read interferogram data
         WRITE(FNAME, '(A,I3.3,A)') 'ifg_', I, '.dat'
         CALL READIFG(FNAME, X, Y, N, IERR)
         IF (IERR .NE. 0) CYCLE
         
         ! Apply apodization
         CALL APOD(X, Y, N, 'HAPP')
         
         ! Perform FFT
         CALL FFT(X, Y, Z, N)
         
         ! Phase correction
         CALL PHCORR(Y, Z, PHASE, N)
         
         ! Convert to spectrum
         CALL MAKSPEC(Y, Z, SPEC, N)
         
         ! Write spectrum
         WRITE(FNAME, '(A,I3.3,A)') 'spec_', I, '.dat'
         CALL WRITESP(FNAME, FREQ, SPEC, N)
      END DO
      
      WRITE(*,*) 'Processing complete'
      
      END PROGRAM FTIR_MAIN