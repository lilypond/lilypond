#ifndef VSMAT_HH
#define VSMAT_HH
#include "vray.hh"

/// a  matrix storage baseclass.
class virtual_smat {
    

public:
    /// check invariants
    virtual void OK() const=0;
    
    /// height of matrix
    virtual int rows() const = 0;

    /// width of matrix
    virtual int cols() const = 0;
    
    /// set the size. contents lost
    virtual void set_size(int i, int j) = 0;
    /**      
      PRE
      i >=0, j>=0
    */
    
    /// set the size to square dimen. contents lost
    virtual void set_size(int i) = 0;
    /**
      PRE
      i>=0
    */
    /// set the size to i
    virtual void resize(int i, int j) = 0;
    /**

      keep contents. If enlarged contents unspecified
        
      PRE
      i>=0, j>=0
    
    */

    /// set the size to square dimen. contents kept
    virtual void resize(int i) = 0;
    /**    
    Keep contents. If enlarged contents are unspecified
    
    PRE
    i>=0  
    */
    
    /// access an element
    virtual Real& elem(int i,int j) = 0;
    /**
    access an element.

    Generate an errormessage, if this happens
    in the 0-part of a sparse matrix.
    */

    /// access a element, no modify
    virtual const Real& elem(int i, int j) const = 0;

#if 1
    virtual svec<Real> row(int i) const = 0;
    virtual svec<Real> column(int j) const = 0;
#endif

    /// add a row
    virtual void insert_row(int k)=0;
    /**
    add a row to the matrix before  row k. Contents
    of added row are unspecified

      0 <= k <= rows()
    */

    /// delete a row 
    virtual void delete_row(int k)=0;
      /**
      delete a row from this matrix.

      PRE
      0 <= k < rows();
    */
    virtual ~virtual_smat() { }
    virtual virtual_smat *clone()=0;


    /// is there a next?
    virtual bool mult_ok(int i, int j) const=0;
    /**
      at end of matrix? when doing loop

      for(i=0; i<h; i++)
        for(j=0; j<w; j++)
          ...

    */
    /// iterate
    virtual void mult_next(int &i, int &j) const  = 0;
    /**
      walk through matrix (regular multiply)
      get next j for row i, or get next row i and reset j.
      this will make sparse matrix implementation easy.
    
      PRE
      mult_ok(i,j)
     */
    virtual bool trans_ok(int i, int j) const=0;
    /**
      valid matrix entry. return false if at end of row
    */
    virtual void trans_next(int &i, int &j) const  = 0;
    /**
      walk through matrix (transposed multiply).
      Get next i (for column j)
    
      PRE
      ver_ok(i,j)
     */

    /// generate a "Full_storage" matrix    
    virtual_smat<Real> *get_full(int n, int m);

};
    
/** base class for interface with matrix storageclasses.  There are no
    iterators for matrixclasses, since matrices are (like arrays)
    explicitly int-indexed.

    Iteration is provided by *_next, *_ok, which update and check both
    index variables simultaneously.

    TODO
    determine type of product matrix.

*/

#endif
