/*
  parray.hh -- declare Pointer_array

  source file of the Flower Library

  (c)  1997--1998 Han-Wen Nienhuys <hanwen@stack.nl>
*/


#ifndef PARRAY_HH
#define PARRAY_HH

#include "varray.hh"

/**
  an array of pointers.

  TODO
  should init to 0. Derive from Array<void*>? 
 */
template<class T>
class Link_array : public Array<T*>
{
    static default_compare (T *const& p1, T  *const&p2) {
	/* can't do p1 -p2, since T might be an incomplete type */
	if (p1 < p2)
	    return -1 ;
	if (p2 < p1)
	    return 1;
	return 0;
    }
public:
    void substitute (T *old, T*new_l)
    {
	int i;
	while ((i = find_i (old)) >=0) 
	    if (new_l)
		elem (i) =new_l;
	    else
		del (i);
    }
    void unordered_substitute (T* old, T * new_l)
    {
	int i;
	while ((i = find_i (old)) >=0) 
	    if (new_l)
		elem (i) =new_l;
	    else {
		unordered_del (i);
	    }
    
    }
    void default_sort() {
	sort (default_compare);
    }
    void uniq() {
	Link_array<T> l_arr;
	for (int i=0; i < size(); i++) 
	    if (!i || elem (i-1) != elem (i))
		l_arr.push (elem (i)); 
	*this = l_arr;
    }

    int find_i (T const * t) const {
	for (int i=0; i < size(); i++)
	    if (elem (i) == t)
		return i;
	return -1;
    }
    T *find_l (T const *t) const
    {
	int i = find_i (t);
	if (i >= 0)
	    return elem (i);
	else
	    return 0;
    }
};

#endif // PARRAY_HH
