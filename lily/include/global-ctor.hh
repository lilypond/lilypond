/*
  global-ctor.hh -- declare Global construction stuff.

  source file of the GNU LilyPond music typesetter

  (c) 1999--2009 Han-Wen Nienhuys <hanwen@xs4all.nl>
*/

#ifndef GLOBAL_CTOR_HH
#define GLOBAL_CTOR_HH

#define ADD_GLOBAL_CTOR_WITHNAME(y, x)		\
  class Global_ctor_ ## y			\
  {						\
  public:					\
    Global_ctor_ ## y ()			\
      {						\
	add_constructor (x);			\
      }						\
  }						\
    _ ## y ## _ctor_init;

#define ADD_GLOBAL_CTOR(x) ADD_GLOBAL_CTOR_WITHNAME (x, x);

typedef void (* Global_ctor) ();
void add_constructor (Global_ctor ctor);
void call_constructors ();

#endif /* GLOBAL_CTOR_HH */

