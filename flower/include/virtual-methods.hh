/*
  virtual-methods.hh -- declare macros for our do-it-yourself RTTI

  source file of the Flower Library

  (c)  1997--2000 Han-Wen Nienhuys <hanwen@cs.uu.nl>
*/


#ifndef VIRTUAL_METHODS_HH
#define VIRTUAL_METHODS_HH

#include <typeinfo>

#define classname(class_ptr)   demangle_classname (typeid (* (class_ptr)))

const char *
demangle_classname (std::type_info const &);

/**

   Virtual copy constructor. Make up for C++'s lack of a standard
   clone () function.  Uses a typeof hack.  Usage:

   class Foo : Baseclass {
   	VIRTUAL_COPY_CONS (Baseclass);
   };
   
 */

/*
  fix constness: gcc-2.95 is correct in defining
  
    typeof (*this)
    
  in a const member function to be const
*/
#define VIRTUAL_COPY_CONS(base) \
  virtual base* clone_const_helper () \
    { \
      return new typeof (*this) (*this); \
    } \
  virtual base* clone () const \
    { \
      base* urg = (base*)this; \
      return urg->clone_const_helper (); \
    }


#endif /* VIRTUAL_METHODS_HH */
