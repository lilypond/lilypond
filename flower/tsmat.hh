#ifndef SMAT_HH
#define SMAT_HH
#include "vray.hh"
#include "vsmat.hh"

/// simplest matrix storage. refer to its baseclass for the doco.
template<class T>
class Full_storage : public virtual_smat<T>
{
    /// height, width
    int h,w;
    /// maxima.
    int maxh, maxw;
    
    /// the storage
    T** els;
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
    
    virtual T& elem(int i,int j) {
	assert(valid(i,j));
	return els[i][j];
    }
    virtual const T& elem(int i, int j) const {
	assert(valid(i,j));
	return els[i][j];
    }
    virtual svec<T> row(int i) const;
    virtual svec<T> column(int j) const;

    Full_storage() {
	init();
    }
    Full_storage(int i, int j) {
	init();
	set_size(i,j);
    }
    Full_storage(int i) {
	init();
	set_size(i);
    }
    void OK() const;
    void operator=(Full_storage const &);
    
    virtual void insert_row(int k);
    virtual void delete_row(int k);

    
    ~Full_storage();
    virtual bool mult_ok(int i, int j)const;
    virtual void mult_next(int &i, int &j) const ;
    virtual bool trans_ok(int i, int j) const;
    virtual void trans_next(int &i, int &j) const;

};

#endif
