/*
  priorities.hh -- declare Priorities

  source file of the Flower Library

  (c)  1997--1998 Han-Wen Nienhuys <hanwen@cs.uu.nl>
*/


#ifndef PRIORITIES_HH
#define PRIORITIES_HH

#include "array.hh"

/**
  A sorted (uni)set. Should connect with PQueue
 */
template<class K>
struct Priorities :    Array<K>
{
    void insert (K k) 
    {
	int i=0;
	for (; i < size(); i++) {
	    if (elem (i) == k)
		return;
	    if (elem (i) > k)
		break;
	}
	Array<K>::insert (k, i);
    }
};
#endif // PRIORITIES_HH

	
