/*
  full-storage.hh -- declare Full_storage

  source file of the Flower Library

  (c) 1997 Han-Wen Nienhuys <hanwen@stack.nl>
*/


#ifndef FULL_STORAGE_HH
#define FULL_STORAGE_HH

#include "varray.hh"
#include "matrix-storage.hh"
#include "real.hh"

/// simplest matrix storage. refer to its baseclass for the doco.
class Full_storage : public Matrix_storage
{
  /// height, width
  int height_i_,width_i_;
  /// maxima.
  int max_height_i_, max_width_i_;
    
    /// the storage
  Real** els_p_p_;

  void
  init() ;
     
  bool valid (int i, int j) const ; 

  void resize_rows (int);
  void resize_cols (int);

public:
  virtual int rows() const;
  virtual int cols() const ;
    
    
  virtual void resize (int i, int j);
  virtual void resize (int i);
  virtual Real& elem (int i,int j);
  virtual Real elem (int i, int j) const ;
  int dim() const;
  Full_storage (Matrix_storage*);
  Full_storage();
  Full_storage (int i, int j);
  Full_storage (Full_storage const&);
  Full_storage (int i);
  void OK() const;
  void operator=(Full_storage const &);
    
  virtual void insert_row (int k);
  virtual void delete_row (int k);
  virtual void delete_column (int k);
    
  ~Full_storage();
  virtual bool mult_ok (int i, int j) const;
  virtual void mult_next (int &i, int &j) const ;
  virtual bool trans_ok (int i, int j) const;
  virtual void trans_next (int &i, int &j) const;
  DECLARE_VIRTUAL_COPY_CONS(Full_storage,Matrix_storage);
  DECLARE_MY_RUNTIME_TYPEINFO;
  virtual bool try_right_multiply (Matrix_storage * dest, Matrix_storage const *) const;
};

#endif // FULL_STORAGE_HH
