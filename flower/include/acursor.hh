/*
  acursor.hh -- declare ACursor, PACursor 

  source file of the Flower Library

  (c)  1997--1998 Han-Wen Nienhuys <hanwen@cs.uu.nl>
*/


#ifndef ACURSOR_HH
#define ACURSOR_HH

template<class T>
class ACursor
{
protected:
    int idx_;
    Array<T> &arr_;
public:
    ACursor (ACursor const& s) 
	:arr_(s.arr_)
    {
	idx_ = s.idx_;
    }
    ACursor (Array<T> const &arr)
	arr_((Array<T>&)arr)
    {
	idx_ =0;
    }
    T thing() const { 
	return arr_[idx_];
    }
    T& thing() { return arr_[idx_]; }
    T& operator++(int) {
	T&t = thing();
	idx_ ++;
	return t;
    }
    bool ok() { return idx_ >=0 && idx_ < arr_.size (); }
};


template<class T>
class PACursor : public ACursor<T*>
{
public:
    PACursor (Link_array<T> l)
	: ACursor (l)
    {
    }
    T* ptr() { return arr_[idx_]; }
    T *operator->() {
	return ptr();
    }
    
};

#endif // ACURSOR_HH
