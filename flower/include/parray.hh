/*
  parray.hh -- declare Pointer_array

  source file of the Flower Library

  (c) 1997 Han-Wen Nienhuys <hanwen@stack.nl>
*/


#ifndef PARRAY_HH
#define PARRAY_HH

#include "varray.hh"

/**
  an array of pointers.

  TODO
  should init to 0.
 */
template<class T>
class Link_array : public Array<T>
{
public:
    int find_i (T t) const{
	for (int i=0; i < size(); i++)
	    if (elem(i) == t)
		return i;
	return -1;
    }
    T find_l(T t)const
    {
	int i = find_i(t);
	if (i >= 0)
	    return elem(i);
	else
	    return 0;
    }
};

#endif // PARRAY_HH
