C     FORTRAN Program: Matrix Operations and Linear Equation Solver
C     Author: Example for FORTRAN to MATLAB Translation
C     Purpose: Demonstrate typical FORTRAN scientific computing code
C     
      PROGRAM MAIN
      IMPLICIT NONE
      INTEGER, PARAMETER :: N = 3
      REAL*8 A(N,N), B(N), X(N)
      INTEGER I, J
      REAL*8 DET
      
C     Initialize coefficient matrix A
      CALL INITMAT(A, N)
      
C     Initialize right-hand side vector B
      CALL INITVEC(B, N)
      
C     Display input matrix and vector
      WRITE(*,*) 'Coefficient Matrix A:'
      CALL PRINTMAT(A, N, N)
      WRITE(*,*) 'Right-hand side vector B:'
      CALL PRINTVEC(B, N)
      
C     Solve linear system Ax = B using Gaussian elimination
      CALL GAUSS(A, B, X, N, DET)
      
C     Display results
      WRITE(*,*) 'Solution vector X:'
      CALL PRINTVEC(X, N)
      WRITE(*,100) DET
100   FORMAT('Determinant = ', F12.6)
      
      STOP
      END

C     Subroutine to initialize matrix with test values
      SUBROUTINE INITMAT(A, N)
      IMPLICIT NONE
      INTEGER N
      REAL*8 A(N,N)
      INTEGER I, J
      
      DO 10 I = 1, N
         DO 20 J = 1, N
            IF (I .EQ. J) THEN
               A(I,J) = 2.0D0 + I
            ELSE
               A(I,J) = 1.0D0 / (I + J)
            ENDIF
20       CONTINUE
10    CONTINUE
      
      RETURN
      END

C     Subroutine to initialize vector with test values
      SUBROUTINE INITVEC(B, N)
      IMPLICIT NONE
      INTEGER N
      REAL*8 B(N)
      INTEGER I
      
      DO 10 I = 1, N
         B(I) = I * 2.0D0
10    CONTINUE
      
      RETURN
      END

C     Subroutine to print matrix
      SUBROUTINE PRINTMAT(A, M, N)
      IMPLICIT NONE
      INTEGER M, N
      REAL*8 A(M,N)
      INTEGER I, J
      
      DO 10 I = 1, M
         WRITE(*,100) (A(I,J), J = 1, N)
10    CONTINUE
100   FORMAT(3F12.6)
      
      RETURN
      END

C     Subroutine to print vector
      SUBROUTINE PRINTVEC(V, N)
      IMPLICIT NONE
      INTEGER N
      REAL*8 V(N)
      INTEGER I
      
      DO 10 I = 1, N
         WRITE(*,200) V(I)
10    CONTINUE
200   FORMAT(F12.6)
      
      RETURN
      END

C     Gaussian elimination with partial pivoting
      SUBROUTINE GAUSS(A, B, X, N, DET)
      IMPLICIT NONE
      INTEGER N
      REAL*8 A(N,N), B(N), X(N), DET
      INTEGER I, J, K, MAXROW
      REAL*8 TEMP, MAXVAL, FACTOR
      
C     Initialize determinant
      DET = 1.0D0
      
C     Forward elimination with partial pivoting
      DO 100 K = 1, N-1
C        Find pivot
         MAXVAL = ABS(A(K,K))
         MAXROW = K
         DO 110 I = K+1, N
            IF (ABS(A(I,K)) .GT. MAXVAL) THEN
               MAXVAL = ABS(A(I,K))
               MAXROW = I
            ENDIF
110      CONTINUE
         
C        Swap rows if necessary
         IF (MAXROW .NE. K) THEN
            DO 120 J = 1, N
               TEMP = A(K,J)
               A(K,J) = A(MAXROW,J)
               A(MAXROW,J) = TEMP
120         CONTINUE
            TEMP = B(K)
            B(K) = B(MAXROW)
            B(MAXROW) = TEMP
            DET = -DET
         ENDIF
         
C        Eliminate column
         DO 130 I = K+1, N
            FACTOR = A(I,K) / A(K,K)
            DO 140 J = K+1, N
               A(I,J) = A(I,J) - FACTOR * A(K,J)
140         CONTINUE
            B(I) = B(I) - FACTOR * B(K)
130      CONTINUE
         
         DET = DET * A(K,K)
100   CONTINUE
      
      DET = DET * A(N,N)
      
C     Back substitution
      X(N) = B(N) / A(N,N)
      DO 200 I = N-1, 1, -1
         TEMP = B(I)
         DO 210 J = I+1, N
            TEMP = TEMP - A(I,J) * X(J)
210      CONTINUE
         X(I) = TEMP / A(I,I)
200   CONTINUE
      
      RETURN
      END