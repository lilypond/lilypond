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
#define NAME_MEMBERS()	\
static char const *static_name();\
virtual bool is_type_b(const char *)const; \
virtual char const *name() const{ return static_name(); } \
int a_stupid_nonexistent_function_to_allow_the_semicolon_come_out()

#define IMPLEMENT_STATIC_NAME(c)\
    char const *c::static_name() { return #c; } 

#define VIRTUAL_COPY_CONS(T, R)\
  virtual R *clone() const { return  new T(*this); } \

#define IMPLEMENT_IS_TYPE_B(D) 							   \
  bool D::is_type_b(const char *s)	const					   \
{ 										   \
    return s == static_name();							   \
}										   
										   
#define IMPLEMENT_IS_TYPE_B1(D,B) 						   \
  bool D::is_type_b(const char *s)const						   \
{ 										   \
    return s == static_name() || B::is_type_b(s);				   \
}										   
#define IMPLEMENT_IS_TYPE_B2(D, BA, BB) 						   \
  bool D::is_type_b(const char *s)	const					   \
{ 										   \
    return s == static_name() || BA::is_type_b(s) || BB::is_type_b(s); \
}

#endif // CLASS-NAME_HH
