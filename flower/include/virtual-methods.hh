/*
  class-name.hh -- declare 

  source file of the LilyPond music typesetter

  (c) 1997 Han-Wen Nienhuys <hanwen@stack.nl>
*/


#ifndef CLASS_NAME_HH
#define CLASS_NAME_HH

/** a macro to declare the classes name as a static and virtual function.
  The static_name() can *not* be inlined (this might have the effect that 
  s->name() != S::static_name(). Overlapping strings need not be merged in C++
 */
#define NAME_MEMBERS(c)	\
static char const *static_name();\
virtual char const *name() const{ return c::static_name(); } \
int a_stupid_nonexistent_function_to_allow_the_semicolon_come_out()

#define IMPLEMENT_STATIC_NAME(c)\
    char const *c::static_name() { return #c; } 

#define VIRTUAL_COPY_CONS(T, R)\
  virtual R *clone() const { return  new T(*this); } \

    
#endif // CLASS-NAME_HH
