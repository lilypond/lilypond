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
    init() {
	els_p_p_=0;
	height_i_=width_i_=max_height_i_=max_width_i_=0;

    }
    
    bool valid(int i, int j) const {
	return (i>=0 && i < height_i_)
	    && (j < width_i_ && j >=0);
    }
    

    void resize_rows(int);
    void resize_cols(int);

public:
    virtual int rows() const {
	return height_i_;
    }
    virtual int cols() const {
	return width_i_;
    }
    
    
    virtual void set_size(int i, int j)
    {
	resize(i,j); //this could be more efficient.
    }
    
    virtual void set_size(int i) {
	set_size(i,i);
    }
    virtual void resize(int i, int j);
    virtual void resize(int i) {
	resize(i,i);
    }
    
    virtual Real& elem(int i,int j) {
	assert(valid(i,j));
	return els_p_p_[i][j];
    }
    virtual Real const & elem(int i, int j) const {
	assert(valid(i,j));
	return els_p_p_[i][j];
    }
    virtual Array<Real> row(int i) const;
    virtual Array<Real> column(int j) const;

    Full_storage() {
	init();
    }
    Full_storage(int i, int j) {
	init();
	set_size(i,j);
    }
    Full_storage(Full_storage&);
    Full_storage(int i) {
	init();
	set_size(i);
    }
    void OK() const;
    void operator=(Full_storage const &);
    
    virtual void insert_row(int k);
    virtual void delete_row(int k);
    virtual void delete_column(int k);

    
    ~Full_storage();
    virtual bool mult_ok(int i, int j)const;
    virtual void mult_next(int &i, int &j) const ;
    virtual bool trans_ok(int i, int j) const;
    virtual void trans_next(int &i, int &j) const;
    virtual Matrix_storage * clone();
    NAME_MEMBERS();
    virtual bool try_right_multiply(Matrix_storage * dest, Matrix_storage const * );
};

#endif // FULL_STORAGE_HH
