#ifndef VSMAT_HH
#define VSMAT_HH
#include "varray.hh"
#include "real.hh"
/** base class for interface with matrix storageclasses.  There are no
    iterators for matrixclasses, since matrices are (like arrays)
    explicitly int-indexed.

    Iteration is provided by *_next, *_ok, which update and check both
    index variables simultaneously.

    TODO
    determine type of product matrix.

*/
class virtual_smat {
    

public:
    /// check invariants
    virtual void OK() const=0;
    
    /// height of matrix
    virtual int rows() const = 0;

    /// width of matrix
    virtual int cols() const = 0;
 
      
    /**  set the size. contents lost.       
      PRE
      i >=0, j>=0
    */
    virtual void set_size(int i, int j) = 0;
 
    /**set the size to square dimen. contents lost
      PRE
      i>=0
    */
    virtual void set_size(int i) = 0;
     /**set the size to i.

      keep contents. If enlarged contents unspecified
        
      PRE
      i>=0, j>=0
    
    */
    virtual void resize(int i, int j) = 0;
 
  /**    
    set the size to square dimen. contents kept
    Keep contents. If enlarged contents are unspecified
    
    PRE
    i>=0  
    */
    virtual void resize(int i) = 0;
  
    
        /**
    access an element.

    Generate an errormessage, if this happens
    in the 0-part of a sparse matrix.
    */

    virtual Real& elem(int i,int j) = 0;

    /// access a element, no modify
    virtual const Real& elem(int i, int j) const = 0;

#if 1
    virtual Array<Real> row(int i) const = 0;
    virtual Array<Real> column(int j) const = 0;
#endif

    
    /**
    add a row to the matrix before  row k. Contents
    of added row are unspecified

      0 <= k <= rows()
    */
    virtual void insert_row(int k)=0;

    
      /**
      delete a row from this matrix.

      PRE
      0 <= k < rows();
    */
    virtual void delete_row(int k)=0;
        virtual void delete_column(int k)=0;
    virtual ~virtual_smat() { }
    virtual virtual_smat *clone()=0;


    
    /**
      at end of matrix?. when doing loop

      for(i=0; i<h; i++)
        for(j=0; j<w; j++)
          ..

    */
    virtual bool mult_ok(int i, int j) const=0;

    /**
      walk through matrix (regular multiply).
      get next j for row i, or get next row i and reset j.
      this will make sparse matrix implementation easy.
    
      PRE
      mult_ok(i,j)
     */
    virtual void mult_next(int &i, int &j) const  = 0;

/**
      valid matrix entry. return false if at end of row
    */
    virtual bool trans_ok(int i, int j) const=0;

    /**
      walk through matrix (transposed multiply).
      Get next i (for column j)
    
      PRE
      ver_ok(i,j)
     */

    virtual void trans_next(int &i, int &j) const  = 0;
    /// generate a "Full_storage" matrix    
    static virtual_smat *get_full(int n, int m);

};
    

#endif
