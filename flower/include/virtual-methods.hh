/*
  virtual-methods.hh -- declare macros for our do-it-yourself RTTI

  source file of the Flower Library

  (c)  1997--1998 Han-Wen Nienhuys <hanwen@cs.uu.nl>
*/


#ifndef VIRTUAL_METHODS_HH
#define VIRTUAL_METHODS_HH

#include <typeinfo>
#include "stdlib.h"		// size_t

/**  Declare the classes name as a static and virtual function.
  The static_name() can *not* be inlined (this might have the effect that 
  s->name() != S::static_name (). Overlapping strings need not be merged in C++
 */
#define DECLARE_MY_RUNTIME_TYPEINFO	\
virtual char const *name() const{ return static_name (); }\
static char const *static_name()


#if 0
  /*
    oops.  before() has nothing to do with inheritance
   */
inline bool operator > (type_info const &a1, type_info const &a2)
{
  return a2.before (a1);
}

inline bool operator < (type_info const &a1, type_info const &a2)
{
  return a1.before (a2);
}

inline bool operator <= (type_info const &a1, type_info const &a2)
{
  return a1 == a2 ||  a1 < a2;
}

inline bool operator >= (type_info const &a1, type_info const &a2)
{
  return a1 == a2 ||  a1 > a2;
}
#endif

#define IMPLEMENT_STATIC_NAME(c)\
    char const *c::static_name() { return #c; }

     /*
    size_t c::static_class_size () { return sizeof (c); }
    */

#define VIRTUAL_COPY_CONS(T, R)\
  virtual R *clone() const { return  new T(*this); } 

#define DECLARE_VIRTUAL_COPY_CONS(T, R)\
      virtual R *clone() const

#define IMPLEMENT_VIRTUAL_COPY_CONS(T, R)\
	R *T::clone() const { return  new T(*this); }\

    
#define IMPLEMENT_IS_TYPE_B(D)\
  IMPLEMENT_STATIC_NAME(D)

/*
  bool D::static_is_type_b (const char *s)\
{\
  return s == static_name();\
}*/

#define IMPLEMENT_IS_TYPE_B1(D, B)\
  IMPLEMENT_STATIC_NAME(D)
/*
  bool D::static_is_type_b (const char *s)\
{\
  return s == static_name() || B::static_is_type_b (s);\
}
*/

#define IMPLEMENT_IS_TYPE_B2(D, BA, BB)\
  IMPLEMENT_STATIC_NAME(D)

/*
     
  bool D::static_is_type_b (const char *s)\
{\
  return s == static_name() || BA::static_is_type_b (s) || BB::static_is_type_b (s);\
}
*/

#endif 
