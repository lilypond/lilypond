/*
  sstack.hh -- part of Flower lib

  (c) 1996 Han-Wen Nienhuys
*/

#ifndef SSTACK_HH
#define SSTACK_HH

#include "varray.hh"

/// A simple stack based on Array.
template<class T>
struct sstack : Array<T> { 
    T top() { return last(); }
    T pop() {
	assert(!empty());
	T l = last();
        Array<T>::pop();
	return l;
    }
    void push(T l) { add(l); }
};
/**
  Same as for #Array# goes here.
*/


#endif // SSTACK_HH

