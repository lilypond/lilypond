/*
  virtual-methods.hh -- declare macros for our do-it-yourself RTTI

  source file of the Flower Library

  (c) 1997--2004 Han-Wen Nienhuys <hanwen@cs.uu.nl>
*/


#ifndef VIRTUAL_METHODS_HH
#define VIRTUAL_METHODS_HH

#include <typeinfo>

#define classname(class_ptr) demangle_classname (typeid (*(class_ptr)))

const char *
demangle_classname (std::type_info const &);

/* Virtual copy constructor.  Make up for C++'s lack of a standard
   factory or clone () function.  Uses a typeof hack.  Usage:

   class Foo : Baseclass
   {
      VIRTUAL_COPY_CONSTRUCTOR (Baseclass, Foo);
   }; */

#define VIRTUAL_COPY_CONSTRUCTOR(base, name) \
  /* Hack to fix constness: gcc >= 2.95 is correct in defining \
     typeof (*this) in a const member function to be const.  */ \
  virtual base* clone_const_helper () \
    { \
      return new name (*this); \
    } \
  virtual base *clone () const \
    { \
      /* return new name (*this); */ \
      base *urg = (base*) this; \
      return urg->clone_const_helper (); \
    }

#endif /* VIRTUAL_METHODS_HH */
