/*
  sstack.hh -- part of Flower lib

  (c) 1996 Han-Wen Nienhuys
*/

#ifndef SSTACK_HH
#define SSTACK_HH

#include "vray.hh"

/// A simple stack based on svec.
template<class T>
struct sstack : svec<T> { 
    T top() { return last(); }
    T pop() {
	assert(!empty());
	T l = last();
        svec<T>::pop();
	return l;
    }
    void push(T l) { add(l); }
};
/**
  Same as for #svec# goes here.
*/


#endif // SSTACK_HH

