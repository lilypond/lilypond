/*
  plist.tcc -- implement Pointer_list

  source file of the Flower Library

  (c)  1997--1998 Han-Wen Nienhuys <hanwen@stack.nl>
*/


#ifndef PLIST_TCC
#define PLIST_TCC

#include "plist.hh"

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

#ifndef __CYGWIN32__ // ugh should check for some gcc/egcs version

#define POINTERLIST_INSTANTIATE(a) template class Pointer_list<a*>;\
	template class PCursor<a*>;

#else

#define POINTERLIST_INSTANTIATE(T)\
    static void force_junk##T ()\
    {\
    Pointer_list<T*> bla;\
    bla.junk ();\
    }

#endif

#endif // PLIST_TCC
