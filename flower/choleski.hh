#ifndef CHOLESKI_HH
#define CHOLESKI_HH

#include "matrix.hh"

/**
    structure for using the LU decomposition of a positive definite .

    #P# is split  into

    LD transpose(L)
    */
struct Choleski_decomposition {

    /// lower triangle of Choleski decomposition
    Matrix L;

    /// diagonal 
    Vector D;
    ///Create decomposition of P
    /**
    PRE
    P needs to be symmetric positive definite
    */
    
    Choleski_decomposition(Matrix P);
    Vector solve(Vector rhs) const;
        /**
    solve Px = rhs
    */
Vector operator * (Vector rhs) const { return solve (rhs); }
  /**
    return the inverse of the matrix P.
    */

    Matrix inverse() const;
    /**
    return P,  calc'ed from L and D
    */
    Matrix original() const;
  
	 
};
#endif
