/*
  virtual-methods.hh -- declare macros for our do-it-yourself RTTI

  source file of the Flower Library

  (c)  1997--1999 Han-Wen Nienhuys <hanwen@cs.uu.nl>
*/


#ifndef VIRTUAL_METHODS_HH
#define VIRTUAL_METHODS_HH

#include <typeinfo>
#include "stdlib.h"		// size_t

#define classname(class_ptr)   demangle_classname(typeid(*(class_ptr)).name())

const char *
demangle_classname (const char*);

/**

   Virtual copy constructor. Make up for C++'s lack of a standard
   clone() function.  Uses a typeof hack.  Usage:

   class Foo : Baseclass {
   	VIRTUAL_COPY_CONS(Baseclass);
   };
   
 */
#define VIRTUAL_COPY_CONS(base)   virtual base *clone () const { return new typeof(*this) (*this); }

#endif 
