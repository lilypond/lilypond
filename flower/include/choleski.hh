#ifndef CHOLESKI_HH
#define CHOLESKI_HH

#include "matrix.hh"

/**
  Choleski decomposition of a matrix
    structure for using the LU decomposition of a positive definite matrix.

    #P# is split  into

    LD transpose (L)
    */
struct Choleski_decomposition {

  /// lower triangle of Choleski decomposition
  Matrix L;

  bool band_b_;

  /// diagonal 
  Vector D;

  /** Create decomposition of P. 
    PRE
    P needs to be symmetric positive definite
    */
    
  Choleski_decomposition (Matrix const &P);

  /**
    solve Px = rhs
    */
  Vector solve (Vector rhs) const;
  void solve (Vector &dest, Vector const &rhs) const;
  Vector operator * (Vector rhs) const { return solve (rhs); }
  /**
    return the inverse of the matrix P.
    */
  Matrix inverse() const;
  /**
    return P,  calc'ed from L and D
    */
  Matrix original() const;
private:
  void full_matrix_solve (Vector &,Vector const&) const;
  void band_matrix_solve (Vector &, Vector const&) const;
  void full_matrix_decompose (Matrix const & P);
  void band_matrix_decompose (Matrix const &P);
	 
};
#endif
