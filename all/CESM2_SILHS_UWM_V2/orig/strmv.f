*> \brief \b STRMV
*
*  =========== DOCUMENTATION ===========
*
* Online html documentation available at
*            http://www.netlib.org/lapack/explore-html/
*
*  Definition:
*  ===========
*
*       SUBROUTINE STRMV(UPLO,TRANS,DIAG,N,A,LDA,X,INCX)
*
*       .. Scalar Arguments ..
*       INTEGER INCX,LDA,N
*       CHARACTER DIAG,TRANS,UPLO
*       ..
*       .. Array Arguments ..
*       REAL A(LDA,*),X(*)
*       ..
*
*
*> \par Purpose:
*  =============
*>
*> \verbatim
*>
*> STRMV  performs one of the matrix-vector operations
*>
*>    x := A*x,   or   x := A**T*x,
*>
*> where x is an n element vector and  A is an n by n unit, or non-unit,
*> upper or lower triangular matrix.
*> \endverbatim
*
*  Arguments:
*  ==========
*
*> \param[in] UPLO
*> \verbatim
*>          UPLO is CHARACTER*1
*>           On entry, UPLO specifies whether the matrix is an upper or
*>           lower triangular matrix as follows:
*>
*>              UPLO = 'U' or 'u'   A is an upper triangular matrix.
*>
*>              UPLO = 'L' or 'l'   A is a lower triangular matrix.
*> \endverbatim
*>
*> \param[in] TRANS
*> \verbatim
*>          TRANS is CHARACTER*1
*>           On entry, TRANS specifies the operation to be performed as
*>           follows:
*>
*>              TRANS = 'N' or 'n'   x := A*x.
*>
*>              TRANS = 'T' or 't'   x := A**T*x.
*>
*>              TRANS = 'C' or 'c'   x := A**T*x.
*> \endverbatim
*>
*> \param[in] DIAG
*> \verbatim
*>          DIAG is CHARACTER*1
*>           On entry, DIAG specifies whether or not A is unit
*>           triangular as follows:
*>
*>              DIAG = 'U' or 'u'   A is assumed to be unit triangular.
*>
*>              DIAG = 'N' or 'n'   A is not assumed to be unit
*>                                  triangular.
*> \endverbatim
*>
*> \param[in] N
*> \verbatim
*>          N is INTEGER
*>           On entry, N specifies the order of the matrix A.
*>           N must be at least zero.
*> \endverbatim
*>
*> \param[in] A
*> \verbatim
*>          A is REAL array, dimension ( LDA, N )
*>           Before entry with  UPLO = 'U' or 'u', the leading n by n
*>           upper triangular part of the array A must contain the upper
*>           triangular matrix and the strictly lower triangular part of
*>           A is not referenced.
*>           Before entry with UPLO = 'L' or 'l', the leading n by n
*>           lower triangular part of the array A must contain the lower
*>           triangular matrix and the strictly upper triangular part of
*>           A is not referenced.
*>           Note that when  DIAG = 'U' or 'u', the diagonal elements of
*>           A are not referenced either, but are assumed to be unity.
*> \endverbatim
*>
*> \param[in] LDA
*> \verbatim
*>          LDA is INTEGER
*>           On entry, LDA specifies the first dimension of A as declared
*>           in the calling (sub) program. LDA must be at least
*>           max( 1, n ).
*> \endverbatim
*>
*> \param[in,out] X
*> \verbatim
*>          X is REAL array, dimension at least
*>           ( 1 + ( n - 1 )*abs( INCX ) ).
*>           Before entry, the incremented array X must contain the n
*>           element vector x. On exit, X is overwritten with the
*>           transformed vector x.
*> \endverbatim
*>
*> \param[in] INCX
*> \verbatim
*>          INCX is INTEGER
*>           On entry, INCX specifies the increment for the elements of
*>           X. INCX must not be zero.
*> \endverbatim
*
*  Authors:
*  ========
*
*> \author Univ. of Tennessee
*> \author Univ. of California Berkeley
*> \author Univ. of Colorado Denver
*> \author NAG Ltd.
*
*> \date December 2016
*
*> \ingroup single_blas_level2
*
*> \par Further Details:
*  =====================
*>
*> \verbatim
*>
*>  Level 2 Blas routine.
*>  The vector and matrix arguments are not referenced when N = 0, or M = 0
*>
*>  -- Written on 22-October-1986.
*>     Jack Dongarra, Argonne National Lab.
*>     Jeremy Du Croz, Nag Central Office.
*>     Sven Hammarling, Nag Central Office.
*>     Richard Hanson, Sandia National Labs.
*> \endverbatim
*>
*  =====================================================================
      SUBROUTINE strmv(UPLO,TRANS,DIAG,N,A,LDA,X,INCX)
*
*  -- Reference BLAS level2 routine (version 3.7.0) --
*  -- Reference BLAS is a software package provided by Univ. of Tennessee,    --
*  -- Univ. of California Berkeley, Univ. of Colorado Denver and NAG Ltd..--
*     December 2016
*
*     .. Scalar Arguments ..
      INTEGER INCX,LDA,N
      CHARACTER DIAG,TRANS,UPLO
*     ..
*     .. Array Arguments ..
      REAL A(lda,*),X(*)
*     ..
*
*  =====================================================================
*
*     .. Parameters ..
      REAL ZERO
      parameter(zero=0.0e+0)
*     ..
*     .. Local Scalars ..
      REAL TEMP
      INTEGER I,INFO,IX,J,JX,KX
      LOGICAL NOUNIT
*     ..
*     .. External Functions ..
      LOGICAL LSAME
      EXTERNAL lsame
*     ..
*     .. External Subroutines ..
      EXTERNAL xerbla
*     ..
*     .. Intrinsic Functions ..
      INTRINSIC max
*     ..
*
*     Test the input parameters.
*
      info = 0
      IF (.NOT.lsame(uplo,'U') .AND. .NOT.lsame(uplo,'L')) THEN
          info = 1
      ELSE IF (.NOT.lsame(trans,'N') .AND. .NOT.lsame(trans,'T') .AND.
     +         .NOT.lsame(trans,'C')) THEN
          info = 2
      ELSE IF (.NOT.lsame(diag,'U') .AND. .NOT.lsame(diag,'N')) THEN
          info = 3
      ELSE IF (n.LT.0) THEN
          info = 4
      ELSE IF (lda.LT.max(1,n)) THEN
          info = 6
      ELSE IF (incx.EQ.0) THEN
          info = 8
      END IF
      IF (info.NE.0) THEN
          CALL xerbla('STRMV ',info)
          RETURN
      END IF
*
*     Quick return if possible.
*
      IF (n.EQ.0) RETURN
*
      nounit = lsame(diag,'N')
*
*     Set up the start point in X if the increment is not unity. This
*     will be  ( N - 1 )*INCX  too small for descending loops.
*
      IF (incx.LE.0) THEN
          kx = 1 - (n-1)*incx
      ELSE IF (incx.NE.1) THEN
          kx = 1
      END IF
*
*     Start the operations. In this version the elements of A are
*     accessed sequentially with one pass through A.
*
      IF (lsame(trans,'N')) THEN
*
*        Form  x := A*x.
*
          IF (lsame(uplo,'U')) THEN
              IF (incx.EQ.1) THEN
                  DO 20 j = 1,n
                      IF (x(j).NE.zero) THEN
                          temp = x(j)
                          DO 10 i = 1,j - 1
                              x(i) = x(i) + temp*a(i,j)
   10                     CONTINUE
                          IF (nounit) x(j) = x(j)*a(j,j)
                      END IF
   20             CONTINUE
              ELSE
                  jx = kx
                  DO 40 j = 1,n
                      IF (x(jx).NE.zero) THEN
                          temp = x(jx)
                          ix = kx
                          DO 30 i = 1,j - 1
                              x(ix) = x(ix) + temp*a(i,j)
                              ix = ix + incx
   30                     CONTINUE
                          IF (nounit) x(jx) = x(jx)*a(j,j)
                      END IF
                      jx = jx + incx
   40             CONTINUE
              END IF
          ELSE
              IF (incx.EQ.1) THEN
                  DO 60 j = n,1,-1
                      IF (x(j).NE.zero) THEN
                          temp = x(j)
                          DO 50 i = n,j + 1,-1
                              x(i) = x(i) + temp*a(i,j)
   50                     CONTINUE
                          IF (nounit) x(j) = x(j)*a(j,j)
                      END IF
   60             CONTINUE
              ELSE
                  kx = kx + (n-1)*incx
                  jx = kx
                  DO 80 j = n,1,-1
                      IF (x(jx).NE.zero) THEN
                          temp = x(jx)
                          ix = kx
                          DO 70 i = n,j + 1,-1
                              x(ix) = x(ix) + temp*a(i,j)
                              ix = ix - incx
   70                     CONTINUE
                          IF (nounit) x(jx) = x(jx)*a(j,j)
                      END IF
                      jx = jx - incx
   80             CONTINUE
              END IF
          END IF
      ELSE
*
*        Form  x := A**T*x.
*
          IF (lsame(uplo,'U')) THEN
              IF (incx.EQ.1) THEN
                  DO 100 j = n,1,-1
                      temp = x(j)
                      IF (nounit) temp = temp*a(j,j)
                      DO 90 i = j - 1,1,-1
                          temp = temp + a(i,j)*x(i)
   90                 CONTINUE
                      x(j) = temp
  100             CONTINUE
              ELSE
                  jx = kx + (n-1)*incx
                  DO 120 j = n,1,-1
                      temp = x(jx)
                      ix = jx
                      IF (nounit) temp = temp*a(j,j)
                      DO 110 i = j - 1,1,-1
                          ix = ix - incx
                          temp = temp + a(i,j)*x(ix)
  110                 CONTINUE
                      x(jx) = temp
                      jx = jx - incx
  120             CONTINUE
              END IF
          ELSE
              IF (incx.EQ.1) THEN
                  DO 140 j = 1,n
                      temp = x(j)
                      IF (nounit) temp = temp*a(j,j)
                      DO 130 i = j + 1,n
                          temp = temp + a(i,j)*x(i)
  130                 CONTINUE
                      x(j) = temp
  140             CONTINUE
              ELSE
                  jx = kx
                  DO 160 j = 1,n
                      temp = x(jx)
                      ix = jx
                      IF (nounit) temp = temp*a(j,j)
                      DO 150 i = j + 1,n
                          ix = ix + incx
                          temp = temp + a(i,j)*x(ix)
  150                 CONTINUE
                      x(jx) = temp
                      jx = jx + incx
  160             CONTINUE
              END IF
          END IF
      END IF
*
      RETURN
*
*     End of STRMV .
*
      END
