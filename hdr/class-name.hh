/*
  class-name.hh -- declare 

  source file of the LilyPond music typesetter

  (c) 1997 Han-Wen Nienhuys <hanwen@stack.nl>
*/


#ifndef CLASS_NAME_HH
#define CLASS_NAME_HH

/// a macro to declare the classes name as a static and virtual function.
#define NAME_MEMBERS(c)	\
static const char *static_name(){ return #c; }\
virtual const char *name() const{ return c::static_name(); } \
int a_stupid_nonexistent_function_to_allow_the_semicolon_come_out()
    
#endif // CLASS-NAME_HH
