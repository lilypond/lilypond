/*
  matrix-storage.hh -- declare Matrix_storage

  source file of the Flower Library

  (c)  1997--1998 Han-Wen Nienhuys <hanwen@cs.uu.nl>
*/


#ifndef MATRIX_STORAGE_HH
#define MATRIX_STORAGE_HH

#include "array.hh"
#include "real.hh"

/** 

    base class for interface with matrix storageclasses.  There are no
    iterators for matrixclasses, since matrices are (like arrays)
    explicitly int-indexed.

    Iteration is provided by *_next, *_ok, which update and check both
    index variables simultaneously.

    TODO
    determine type of product matrix.

*/
class Matrix_storage {
    

public:
  /// check invariants
  void OK() const;
    
  /// height of matrix
  int rows() const;

  /// width of matrix
  int cols() const;

  /// size if square
  int dim() const;
      
  /**  set the size. contents lost.       
       PRE
       i >=0, j>=0
  */
  void set_size (int rows, int cols) ;

 
  /**set the size to square dimen. contents lost
     PRE
     i>=0
  */
  void set_size (int i) ;
  /**set the size to i.

     keep contents. If enlarged contents unspecified
        
     PRE
     i>=0, j>=0
    
  */
  void resize (int rows, int cols);
 
  /**    
	 set the size to square dimen. contents kept
	 Keep contents. If enlarged contents are unspecified
    
	 PRE
	 i>=0  
  */
  void resize (int i);
  
    
  /**
     access an element.

     Generate an errormessage, if this happens
     in the 0-part of a sparse matrix.
  */

  Real& elem (int i,int j);

  /// access a element, no modify
  Real elem (int i, int j) const;

  Array<Real> row (int i) const ;
  Array<Real> column (int j) const;

    
  /**
     add a row to the matrix before  row k. Contents
     of added row are unspecified

     0 <= k <= rows()
  */
  void insert_row (int k);

    
  /**
     delete a row from this matrix.

     PRE
     0 <= k < rows();
  */
  void delete_row (int k);
  void delete_column (int k);
  ~Matrix_storage() { }

    
  /**
     at end of matrix?. when doing loop

     for (i=0; i<h; i++)
     for (j=0; j<w; j++)
     ..

  */
  bool mult_ok (int i, int j) const;

  /**
     walk through matrix (regular multiply).
     get next j for row i, or get next row i and reset j.
     this will make sparse matrix implementation easy.
    
     PRE
     mult_ok (i,j)
  */
  void mult_next (int &i, int &j) const;

  /**
     valid matrix entry. return false if at end of row
  */
  bool trans_ok (int i, int j) const;

  /**
     walk through matrix (transposed multiply).
     Get next i (for column j)
    
     PRE
     ver_ok (i,j)
  */

  void trans_next (int &i, int &j) const;
    
  /// generate a "Full_storage" matrix    
  static void set_band (Matrix_storage*&, int band);
};

#endif // MATRIX_STORAGE_HH

