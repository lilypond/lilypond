#ifndef SMAT_HH
#define SMAT_HH
#include "varray.hh"
#include "vsmat.hh"
#include "real.hh"
/// simplest matrix storage. refer to its baseclass for the doco.
class Full_storage : public virtual_smat
{
    /// height, width
    int h,w;
    /// maxima.
    int maxh, maxw;
    
    /// the storage
    Real** els;
    void
    init() {
	els=0;
	h=w=maxh=maxw=0;

    }
    
    bool valid(int i, int j) const {
	return (i>=0 && i < h)
	    && (j < w && j >=0);
    }
    

    void resize_rows(int);
    void resize_cols(int);

public:
    virtual int rows() const {
	return h;
    }
    virtual int cols() const {
	return w;
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
	return els[i][j];
    }
    virtual Real const & elem(int i, int j) const {
	assert(valid(i,j));
	return els[i][j];
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
    virtual virtual_smat * clone();
};

#endif
