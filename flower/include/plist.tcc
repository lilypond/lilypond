/*
  plist.tcc -- implement Pointer_list

  source file of the Flower Library

  (c) 1997 Han-Wen Nienhuys <hanwen@stack.nl>
*/


#ifndef PLIST_TCC
#define PLIST_TCC

#include "plist.hh"

#define POINTERLIST_INSTANTIATE(a) class Pointer_list<a*>;\
	template class PCursor<a*>;
	
template<class T>
void
Pointer_list<T>::junk()
{
  PCursor<T> c (*this);
  while (c.ok()) 
    {
	delete c.remove_p();
    }
}

#endif // PLIST_TCC
