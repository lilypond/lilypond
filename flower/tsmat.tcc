#include "smat.hh"

template<class T>
void
Full_storage<T>::operator=(Full_storage const &fs)
{
    resize(fs.h, fs.w);
    for (int i=0; i<h; i++)
	for (int j=0; i<w; j++)
	    els[i][j]= fs.els[i][j];
}

template<class T>
void
Full_storage<T>::OK() const
{
    assert(maxh >= h && maxw >= w);
    assert(h >= 0 && w >= 0);
}
template<class T>
void
Full_storage<T>::resize_cols(int newh)
{
    if (newh <= maxh) {
	h=newh;
	return;
    }
   
    T** newa=new T*[newh];
    int j=0;
    for (; j < h; j++)
	newa[j] = els[j];
    for (; j < newh; j++)
	newa[j] = new T[w];
    delete[] els;
    els=newa;
    maxh = newh;
}

template<class T>
void
Full_storage<T>::resize_rows(int neww)
{
    if (neww <= maxw) {
	w=neww;
	return;
    }
    for (int i=0; i < h ; i++) {
	T* newa=new T[neww];
	for (int k=0; k < w; k++)
	    newa[k] = els[i][k];

	delete[] els[i];
	els[i] = newa;
	maxw = neww;
    }
}

template<class T>
Full_storage<T>::~Full_storage() {
    for (int i=0; i < maxh; i++)
	delete [] els[i];
    delete[] els;
}

template<class T>
void
Full_storage<T>::resize(int i, int j)
{
    resize_cols(i);
    resize_rows(j);
}

template<class T>
void
Full_storage<T>::set_size(int i, int j)
{    
    resize(i,j)
}

template<class T>
bool
Full_storage<T>::mult_ok(int i, int j) const
{
    return valid(i,j);
}

template<class T>
bool
Full_storage<T>::trans_ok(int i, int j) const
{
       return valid(i,j);
} 


template<class T>
void
Full_storage<T>::trans_next(int &i, int &j) const
{
    assert(trans_ok(i,j));
    i++;
    if (i >= h) {
	i=0;
	j ++;
    }
}

template<class T>
void
Full_storage<T>::mult_next(int &i, int &j) const
{
    assert(mult_ok(i,j));
    j++;
    if (j >= w) {
	j=0;
	i++;
    }
}

template<class T>
void
Full_storage<T>::delete_row(int k)
{
    assert(0 <= k <h);
    for (int i=h-1; i > k ; i++)
	for (int j=0; j < w; j++)
	    els[i-1][j]=els[i][j];
}

template<class T>
void
Full_storage<T>::insert_row(int k)
{
    assert(0 <= k <=h);
    resize_cols(h+1);
    for (int i=h-1; i > k ; i++)
	for (int j=0; j <w; j++)
	    els[i][j]=els[i-1][j];
}

/****************************************************************/

template<class T>
virtual_smat<T> *
virtual_smat<T>::get_full(int n, int m)
{
    return new Full_storage<T>(n,m);
}
#include "real.hh"

template Full_storage<Real>;
