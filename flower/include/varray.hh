/*
  (c) Han-Wen Nienhuys 1995,96,97

  Distributed under GNU GPL  
*/

#ifndef ARRAY_H
#define ARRAY_H
#include <assert.h>

#ifndef INLINE
#define INLINE inline
#endif

/// copy a bare (C-)array from #src# to #dest# sized  #count#
template<class T> void arrcpy (T*dest, T*src, int count);

/**
  Scaleable array/stack template, for a type T with default constructor.
  
  
  This template implements a scaleable vector. With (or without) range
  checking. The type T should have a default constructor. It is
  best suited for simple types, such as int, double or String, it
  provides a paranoidly safe replacement for the new T[int] construct.

  You should \bf{never} store pointers to objects in an Array (since
  the array may be relocated without the pointer knowing it).
  
  It uses stack terminology, (push, pop, top), and  can be used as a stack.

  
  */
template<class T>
class Array
{
protected:
  /// maximum length of array.
  int max_;

  /// the data itself
  T *array_p_;

  /// stretch or shrink  array.
  void remax (int newmax) 
    {	 
      T* newarr = new T[newmax];
      size_ = (newmax < size_) ? newmax : size_;
      arrcpy (newarr, array_p_, size_);
	
      delete[] array_p_;
      array_p_ = newarr;
      max_ = newmax;
    }
  int size_;

public:
  /// check invariants
  void OK() const 
    {
      assert (max_ >= size_ && size_ >=0);
      if (max_) assert (array_p_);
    }
  /** report the size_.
      @see 
      {setsize_}
  */
  int size() const  
    { return size_; }
    
  /// POST: size() == 0
    void clear() 
    { size_ = 0; }

  Array() 
    { array_p_ = 0; max_ =0; size_ =0; }

  // ugh, get around gcc 2.8.1 ice; see bezier.cc
  Array (int i) 
  { 
    max_ = size_ = i; 
    array_p_ = new T[i];
  }


  /** set the size_ to #s#.
      POST: size() == s.
      Warning: contents are unspecified */
  void set_size (int s) 
    {
      if (s >= max_) remax (s);
      size_ = s;    
    }
    
  ~Array() 
    { delete[] array_p_; }

  /// return a  "new"ed copy of array 
    T* copy_array() const 
    {
      T* Tarray = new T[size_];
      arrcpy (Tarray, array_p_, size_);
      return Tarray;
    }
  // depracated
  operator T*() const 
    {
      return copy_array();	
    }
  void operator=(Array const & src) 
    {
      set_size (src.size_);
      arrcpy (array_p_,src.array_p_, size_);
    }
  Array (Array const & src) 
    {
      array_p_ = src.copy_array();
      max_ = size_ = src.size_;	
    }

  /// tighten array size_.
    void precompute()     {
      remax (size_);
    }
    
  T * remove_array_p () {
    T * p = array_p_;
    size_ = 0;
    max_ = 0;
    array_p_ =0;
    return p;
  }

  /// access element
  T &operator[] (int i)  
    {
      return elem (i);
    }
  /// access element
    T const & operator[] (int i) const 
    {
      return elem (i);
    }
  /// access element
    T &elem (int i) const 
    {
      assert (i >=0&&i<size_);
      return ((T*)array_p_)[i];	
    }

  /// add to the end of array
    void push (T x) 
    {
      if (size_ == max_)
	remax (2*max_ + 1);

      // T::operator=(T &) is called here. Safe to use with automatic
      // vars
      array_p_[size_++] = x;
    }
  /// remove and return last entry 
    T pop() 
    {
      assert (!empty());
      T l = top (0);
      set_size (size()-1);
      return l;
    }
  /// access last entry
    T& top (int j=0) 
    {
      return (*this)[size_-j-1];
    }
  /// return last entry
    T top (int j=0) const 
    {
      return (*this)[size_-j-1];
    }


  void swap (int i,int j) 
    {
      T t ((*this)[i]);
      (*this)[i]=(*this)[j];
      (*this)[j]=t;
    }
  bool empty() const 
    { return !size_; }

  void insert (T k, int j);
  /**
     remove  i-th element, and return it.
  */
  T get (int i) 
    {
      T t = elem (i);
      del (i);
      return t;
    }
  void unordered_del (int i)
    
    {
      elem (i) = top();
      set_size (size() -1);
    }
  void del (int i) 
    {
      assert (i >=0&& i < size_);
      arrcpy (array_p_+i, array_p_+i+1, size_-i-1);
      size_--;
    }
  // quicksort.
  void sort (int (*compare)(T const&,T const&),
	     int lower = -1, int upper = -1);
  void concat (Array<T> const &src) 
    {
      int s = size_;
      set_size (size_ + src.size_);
      arrcpy (array_p_+s,src.array_p_, src.size_);	
    }
  Array<T> slice (int lower, int upper) 
    {
      assert (lower >= 0 && lower <=upper&& upper <= size_);
      Array<T> r;
      int s =upper-lower;
      r.set_size (s);
      arrcpy (r.array_p_, array_p_  + lower, s);
      return r;
    }
  void reverse();
};

#include "varray.icc"

#endif
