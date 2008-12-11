/*
  parray.hh -- declare Pointer_array

  source file of the Flower Library

  (c) 1997--2008 Han-Wen Nienhuys <hanwen@xs4all.nl>
*/

#ifndef PARRAY_HH
#define PARRAY_HH

#include "std-vector.hh"

using namespace std;

template<class T>
class Link_array : public vector<T *>
{
  
};

#endif // PARRAY_HH

