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

Actually, this sux. Should use a template struct PQuee_ent<V,I>
 */

template<class V, class I>
struct PQueue
{
    Array<V> value_arr_;
    Array<I> indices_arr_;
    void OK() const 
    {
	
	assert(value_arr_.size() == indices_arr_.size());
    }
    
    void enter(V v, I idx) {
	int j=0;
	for (; j < value_arr_.size(); j++)
	    if (indices_arr_[j] > idx) 
		break;

	insert(j,v,idx);
	
    }
    int size() { return value_arr_.size(); }
    V front_val() { return value_arr_[0]; }
    I front_idx() { return indices_arr_[0]; }
    void del(int i) 
    {
	value_arr_.del(i);
	indices_arr_.del(i);
    }
    int size() const
    {
	OK();
	return value_arr_.size();
    }
    

    void insert(int j, V v, I idx)
    {
	value_arr_.insert(v, j);
	indices_arr_.insert(idx, j);
    }
    


    V get() {
	V retval = front_val();
	del(0);
	return retval;
    }
    
};
#endif // PQUEUE_HH
