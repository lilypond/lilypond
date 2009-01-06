/*
  virtual-methods.hh -- declare macros for our do-it-yourself RTTI

  source file of the Flower Library

  (c) 1997--2009 Han-Wen Nienhuys <hanwen@xs4all.nl>
*/

#ifndef VIRTUAL_METHODS_HH
#define VIRTUAL_METHODS_HH

#include <typeinfo>
using namespace std;

#define classname(class_ptr) demangle_classname (typeid (* (class_ptr)))

char const *
demangle_classname (type_info const &);

/*

Virtual copy constructor.  Make up for C++'s lack of a standard
factory or clone () function.  Usage:

class Foo : Baseclass
{
VIRTUAL_COPY_CONSTRUCTOR (Baseclass, Foo);
};

*/

#define DECLARE_CLASSNAME(name) \
  virtual const char *class_name () const {	\
    return #name; \
}

#define VIRTUAL_COPY_CONSTRUCTOR(Base, name)	\
  DECLARE_CLASSNAME(name);\
  virtual Base *clone () const			\
  {						\
    return new name (*this);			\
  }

#endif /* VIRTUAL_METHODS_HH */
