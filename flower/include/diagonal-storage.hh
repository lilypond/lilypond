/*
  diagonal-storage.hh -- declare Diagonal_storage

  source file of the Flower Library

  (c)  1997--1998 Han-Wen Nienhuys <hanwen@stack.nl>
*/


#ifndef DIAGONAL_STORAGE_HH
#define DIAGONAL_STORAGE_HH
#include "full-storage.hh"

/**
  Store a  matrix with a single-band.

  @invariant
  
  Diagonal_storage (i,j) == band_(i, j-i + band_size_i())
  
  band_.cols() == 2 * band_size_i () + 1
  
 */
class Diagonal_storage : public Matrix_storage {
  Full_storage band_;

public:
  void set_band_size (int b);
  int band_size_i() const;
    
  void assert_valid (int i, int j) const;
  bool band_elt_b (int,int) const;
  void resize_dim (int);
    
  virtual void resize_rows (int d) { resize_dim (d); }
  virtual void resize_cols (int d) { resize_dim (d); }
  virtual int dim() const;

  virtual int rows() const ;
  virtual int cols() const ;
    
    
  virtual void resize (int i, int j);
  virtual void resize (int i);
    
  virtual Real& elem (int i,int j);
  virtual Real elem (int i, int j) const;
  Diagonal_storage (Matrix_storage* , int band_i);
  Diagonal_storage();
  void OK() const;
    
  virtual void insert_row (int k);
  virtual void delete_row (int k);
  virtual void delete_column (int k);
    
  ~Diagonal_storage();
  virtual bool mult_ok (int i, int j) const;
  virtual void mult_next (int &i, int &j) const ;
  virtual bool trans_ok (int i, int j) const;
  virtual void trans_next (int &i, int &j) const;
  DECLARE_VIRTUAL_COPY_CONS(Diagonal_storage, Matrix_storage);
  DECLARE_MY_RUNTIME_TYPEINFO;
  virtual bool try_right_multiply (Matrix_storage * dest, Matrix_storage const *) const;
};
#endif // DIAGONAL_STORAGE_HH
