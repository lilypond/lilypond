/*
  pqueue.hh -- declare 

  source file of the LilyPond music typesetter

  (c) 1997 Han-Wen Nienhuys <hanwen@stack.nl>
*/


#ifndef PQUEUE_HH
#define PQUEUE_HH

#include "varray.hh"

/**
  Stupid Prioq. Should use Lists and STL.
  Smallest is put at the front.
 */

template<class V, class I>
struct PQueue
{
    Array<V> value_arr_;
    Array<I> indices_arr_;

    void enter(V v, I idx) {
	int j=0;
	for (; j < value_arr_.size(); j++)
	    if (indices_arr_[j] > idx) 
		break;

	value_arr_.insert(v, j);
	indices_arr_.insert(idx, j);
    }
    int size() { return value_arr_.size(); }
    V front_val() { return value_arr_[0]; }
    I front_idx() { return indices_arr_[0]; }
    V get() {
	V retval = front_val();
	value_arr_.del(0);
	indices_arr_.del(0);
	return retval;
    }
    
};
#endif // PQUEUE_HH
