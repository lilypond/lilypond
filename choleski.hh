#ifndef CHOLESKI_HH
#define CHOLESKI_HH

#include "matrix.hh"

struct Choleski_decomposition {

    /// lower triangle of Choleski decomposition
    Matrix L;

    /// diagonal 
    Vector D;
    ///Create decomposition of P
    Choleski_decomposition(Matrix P);
    /**
    PRE
    P needs to be symmetric positive definite
    */
    
    Vector solve(Vector rhs) const;
    Vector operator * (Vector rhs) const { return solve (rhs); }
    /**
    solve Px = rhs
    */

    Matrix inverse() const;
    /**
    return the inverse of the matrix P.
    */

    Matrix original() const;
    /**
    return P,  calc'ed from L and D
    */
	 
};
/**
    structure for using the LU decomposition of a positive definite .

    #P# is split  into

    LD transpose(L)
    */
    

#endif
