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
	size = (newmax < size) ? newmax : size;
	arrcpy(newarr, thearray, size);
	
	delete[] thearray;
	thearray = newarr;
	max = newmax;
    }
    int size;

public:
    /// check invariants
    void OK() const {
	assert(max >= size && size >=0);
	if (max) assert(thearray);
    }
    /// report the size. See {setsize}
    int sz() const  { return size; }

    /// POST: sz() == 0
    void clear() { size = 0; }

    svec() { thearray = 0; max =0; size =0; }
    /// set the size to #s#
    void set_size(int s) {
	if (s >= max) remax(s);
	size = s;    
    }
    /** POST: sz() == s.
    Warning: contents are unspecified */
    
    ~svec() { delete[] thearray; }

    /// return a  "new"ed copy of array 
    T* copy_array() const {
	T* Tarray = new T[size];
	arrcpy(Tarray, thearray, size);
	return Tarray;
    }
    // depracated
    operator T* () const {
	return copy_array();	
    }
    void operator=(svec const & src) {
	set_size (src.size);
	arrcpy(thearray,src.thearray, size);
    }
    svec(const svec & src) {
	thearray = src.copy_array();
	max = size = src.size;	
    }

    /// tighten array size.
    void precompute () { remax(size); }

    /// this makes svec behave like an array
    T &operator[] (const int i) const {
	assert(i >=0&&i<size);
	return ((T*)thearray)[i];	
    }

    /// add to the end of array
    void add(T x) {
	if (size == max)
	    remax(2*max + 1);

	// T::operator=(T &) is called here. Safe to use with automatic
	// vars
	thearray[size++] = x;
    }

    /// junk last entry.
    void pop() { size -- ; }

    /// return last entry
    T& last(int j=0) {
	return (*this)[size-j-1];
    }
    void swap (int i,int j) {
	T t((*this)[i]);
	(*this)[i]=(*this)[j];
	(*this)[j]=t;
    }
    bool empty() { return !size; }
    void insert(T&k, int j) {
	assert(j >=0 && j<= size);
	set_size(size+1);
	for (int i=size-1; i > j; i--)
	    thearray[i] = thearray[i-1];
	thearray[j] = k;
    }
    void del(int i) {
	assert(i >=0&& i < size);
	arrcpy(thearray+i, thearray+i+1, size-i);
	size--;
    }
    // quicksort.
    void sort (int (*compare)(T& , T& ),
	       int lower = -1, int upper = -1 ) {
	if (lower < 0) {
	    lower = 0 ;
	    upper = sz();
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
};
/**

  This template implements a scaleable vector. With (or without) range
  checking. It may be flaky for objects with complicated con- and
  destructors. The type T should have a default constructor. It is
  best suited for simple types, such as int, double or String

  */

/// A simple stack based on svec.
template<class T>
class sstack : svec<T> {
 public:
    T top() { return last(); }
    T pop() {
	assert(!empty());
	T l = last();
        svec<T>::pop();
	return l;
    }
    void push(T l) { add(l); }
    bool empty() { return svec<T>::empty(); } 
};
/**
  Same as for #svec# goes here.
*/
#endif
