/*
  (c) Han-Wen Nienhuys 1995,96

  Distributed under GNU GPL  
*/

#ifndef SVEC_H
#define SVEC_H
#include <assert.h>

/// copy a bare (C-)array from #src# to #dest# sized  #count#
template<class T>
inline void arrcpy(T*dest, T*src, int count) {
    for (int i=0; i < count ; i++)
	*dest++ = *src++;
}

///scaleable array template, for T with def ctor.
template<class T>
class svec {
protected:
    
    int max;

    /// the data itself
    T *thearray;

    /// stretch or shrink  array.
    void remax(int newmax) {	 
	T* newarr = new T[newmax];
	size_ = (newmax < size_) ? newmax : size_;
	arrcpy(newarr, thearray, size_);
	
	delete[] thearray;
	thearray = newarr;
	max = newmax;
    }
    int size_;

public:
    /// check invariants
    void OK() const {
	assert(max >= size_ && size_ >=0);
	if (max) assert(thearray);
    }
    /// report the size_. See {setsize_}

    int size() const  { return size_; }
    int sz() const  { return size(); }
    
    /// POST: size() == 0
    void clear() { size_ = 0; }

    svec() { thearray = 0; max =0; size_ =0; }

    /// set the size_ to #s#
    void set_size(int s) {
	if (s >= max) remax(s);
	size_ = s;    
    }
    /** POST: sz() == s.
    Warning: contents are unspecified */
    
    ~svec() { delete[] thearray; }

    /// return a  "new"ed copy of array 
    T* copy_array() const {
	T* Tarray = new T[size_];
	arrcpy(Tarray, thearray, size_);
	return Tarray;
    }
    // depracated
    operator T* () const {
	return copy_array();	
    }
    void operator=(svec const & src) {
	set_size (src.size_);
	arrcpy(thearray,src.thearray, size_);
    }
    svec(const svec & src) {
	thearray = src.copy_array();
	max = size_ = src.size_;	
    }

    /// tighten array size_.
    void precompute () { remax(size_); }

    /// this makes svec behave like an array
    T &operator[] (const int i) const {
	assert(i >=0&&i<size_);
	return ((T*)thearray)[i];	
    }

    /// add to the end of array
    void add(T x) {
	if (size_ == max)
	    remax(2*max + 1);

	// T::operator=(T &) is called here. Safe to use with automatic
	// vars
	thearray[size_++] = x;
    }

    /// junk last entry.
    void pop() { size_ -- ; }

    /// return last entry
    T& last(int j=0) {
	return (*this)[size_-j-1];
    }
    T last(int j=0) const {
	return (*this)[size_-j-1];
    }
    void swap (int i,int j) {
	T t((*this)[i]);
	(*this)[i]=(*this)[j];
	(*this)[j]=t;
    }
    bool empty() { return !size_; }
    void insert(T k, int j) {
	assert(j >=0 && j<= size_);
	set_size(size_+1);
	for (int i=size_-1; i > j; i--)
	    thearray[i] = thearray[i-1];
	thearray[j] = k;
    }
    void del(int i) {
	assert(i >=0&& i < size_);
	arrcpy(thearray+i, thearray+i+1, size_-i-1);
	size_--;
    }
    // quicksort.
    void sort (int (*compare)(T& , T& ),
	       int lower = -1, int upper = -1 ) {
	if (lower < 0) {
	    lower = 0 ;
	    upper = sz()-1;
	}
	if (lower >= upper)
	    return;
	swap(lower, (lower+upper)/2);
	int last = lower;
	for (int i= lower +1; i <= upper; i++)
	    if (compare(thearray[i], thearray[lower]) < 0 )
		swap( ++last,i);
	swap(lower, last);
	sort(compare, lower, last-1);
	sort(compare, last+1, lower);
    }
    void concat(svec<T> const &src) {
	int s = size_;
	set_size(size_ + src.size_);
	arrcpy(thearray+s,src.thearray, src.size_);	
    }
    svec<T> subvec(int lower, int upper) {
	assert(lower >= 0 && lower <=upper&& upper <= size_);
	svec<T> r;
	int s =upper-lower;
	r.set_size(s);
	arrcpy(r.thearray, thearray  + lower, s);
	return r;
    }
};
/**

  This template implements a scaleable vector. With (or without) range
  checking. It may be flaky for objects with complicated con- and
  destructors. The type T should have a default constructor. It is
  best suited for simple types, such as int, double or String

  */

#endif
