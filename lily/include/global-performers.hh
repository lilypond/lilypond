
/*
  global-performers.hh -- declare global performer stuff

  source file of the GNU LilyPond music typesetter

  (c) 1996, 1997 Han-Wen Nienhuys <hanwen@stack.nl>
                 Jan Nieuwenhuizen <jan@digicash.com>
*/

#ifndef GLOBAL_PERFORMER_HH
#define GLOBAL_PERFORMER_HH

/**
  A macro to automate administration of performers
 */
#define ADD_THIS_PERFORMER( c ) \
struct c ## init { \
    static Performer* globalctor () \
    { \
	return new c;\
    } \
    c ## init () \
    { \
	add_Performer( c::static_name(), globalctor ); \
    } \
} _ ## c ## init;

// typedef Performer*(*Perf_ctor)(void); c++ ?
typedef Performer*(*Perf_ctor)();
void add_Performer(String s, Perf_ctor f);

#endif // GLOBAL_PERFORMER_HH 
