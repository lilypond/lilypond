/*
  lily-guile.hh encapsulate guile

  source file of the GNU LilyPond music typesetter

  (c) 1998--2002 Jan Nieuwenhuizen <janneke@gnu.org>
*/

#ifndef LILY_GUILE_HH
#define LILY_GUILE_HH


/*
  TODO: the  GH interface is deprecated as of GUILE 1.6

  Remove all gh_XXX functions.
 */
#include <guile/gh.h>


/* GUILE only includes version in headers (libguile/version.h) as of
   1.5.x.  For some strange reason, they call it SCM.*VERSION.

   Not including config.h here, saves a lot of unnecessary
   recompiles. */
#include <libguile.h>

#ifndef GUILE_MAJOR_VERSION
#ifdef SCM_MAJOR_VERSION
#define GUILE_MAJOR_VERSION SCM_MAJOR_VERSION
#define GUILE_MINOR_VERSION SCM_MINOR_VERSION
#define GUILE_PATCH_LEVEL SCM_MICRO_VERSION
#else
#include "config.h"
#endif
#endif

#include "drul-array.hh"

/* Guile 1.4.x compatibility */
#if GUILE_MINOR_VERSION < 5

#define scm_t_bits scm_bits_t

#define fix_guile_1_4_gh_scm2newstr(str, lenp) gh_scm2newstr (str, (int*)lenp)
#define gh_scm2newstr(str, lenp) fix_guile_1_4_gh_scm2newstr (str, lenp)

#define fix_guile_1_4_scm_primitive_eval(form) scm_eval_3 (form, 1, SCM_EOL)
#define scm_primitive_eval(form) fix_guile_1_4_scm_primitive_eval (form)

#define scm_current_module() (SCM)0
#define scm_set_current_module(x) (void)x
#define scm_c_resolve_module(x) (SCM)0

inline SCM scm_c_make_vector  (int k, SCM val) {
  return scm_make_vector (SCM_MAKINUM (k), val);
}
#define scm_c_define_gsubr scm_make_gsubr
#define scm_c_eval_string(str) gh_eval_str ((char*)str)

#define scm_gc_protect_object scm_protect_object
#define scm_gc_unprotect_object scm_unprotect_object
#define scm_list_n scm_listify
#define SCM_STRING_CHARS SCM_CHARS
#define SCM_STRING_LENGTH SCM_LENGTH
#define SCM_SYMBOL_CHARS SCM_CHARS
#define SCM_SYMBOL_LENGTH SCM_LENGTH
#define SCM_VECTOR_LENGTH SCM_LENGTH

#endif

#if GUILE_MINOR_VERSION < 7
#define scm_gc_unregister_collectable_memory(a,b,c) scm_done_free(b)
#define scm_gc_register_collectable_memory(a,b,c) scm_done_malloc(b)
#endif

#include "direction.hh"
#include "flower-proto.hh"

#ifndef SCM_PACK
#define SCM_PACK(x) ((SCM) x)

#endif
#ifndef SCM_UNPACK
#define SCM_UNPACK(x) ( x)
#endif

/*
  conversion functions follow the GUILE naming convention, i.e.

    A ly_B2A (B b);
 */

SCM ly_last (SCM list);
SCM ly_str02scm (char const*c);
SCM ly_write2scm (SCM s);
SCM ly_deep_copy (SCM);
SCM ly_truncate_list (int k, SCM l );


#if (__GNUC__ > 2)
/*
  todo: should add check for x86 as well
 */
#define CACHE_SYMBOLS
#endif


/*
  We don't use gh_symbol2scm directly, since it has const-correctness
  problems in GUILE 1.3.4
  
 */
SCM my_gh_symbol2scm (const char* x);

#ifdef CACHE_SYMBOLS


/*
  Using this trick we cache the value of gh_symbol2scm ("fooo") where
  "fooo" is a constant string. This is done at the cost of one static
  variable per ly_symbol2scm() use, and one boolean evaluation for
  every call.

  The overall speedup of lily is about 5% on a run of wtk1-fugue2

*/
#define ly_symbol2scm(x) ({ static SCM cached;  \
 SCM value = cached;  /* We store this one locally, since G++ -O2 fucks up else */   \
 if ( __builtin_constant_p ((x)))\
 {  if (!cached)\
     value = cached =  scm_gc_protect_object (my_gh_symbol2scm((x)));\
 } else\
  value = gh_symbol2scm ((char*) (x)); \
  value; })
#else
inline SCM ly_symbol2scm(char const* x) { return my_gh_symbol2scm((x)); }
#endif 



String ly_scm2string (SCM s);
String ly_symbol2string (SCM);
SCM ly_offset2scm (Offset);
Offset ly_scm2offset (SCM);
SCM ly_assoc_chain (SCM key, SCM achain);
SCM ly_assoc_cdr (SCM key, SCM alist);
Interval ly_scm2interval (SCM);
SCM ly_interval2scm (Drul_array<Real>);

void taint (SCM *);


SCM ly_parse_scm (char const* s, int* n);
SCM ly_quote_scm (SCM s);
SCM ly_type (SCM);
bool type_check_assignment (SCM val, SCM sym,  SCM type_symbol) ;
String print_scm_val (SCM val);
SCM ly_number2string (SCM s);

SCM parse_symbol_list (char const *);

inline SCM ly_cdr (SCM x) { return SCM_CDR (x); }
inline SCM ly_car (SCM x) { return SCM_CAR (x); } 
inline SCM ly_caar (SCM x) { return SCM_CAAR (x); }
inline SCM ly_cdar (SCM x) { return SCM_CDAR (x); }
inline SCM ly_cadr (SCM x) { return SCM_CADR (x); }
inline SCM ly_cddr (SCM x) { return SCM_CDDR (x); }
inline SCM ly_caddr (SCM x) { return SCM_CADDR (x); }
inline SCM ly_cdadr (SCM x) { return SCM_CDADR (x); }
inline SCM ly_caadr (SCM x) { return SCM_CAADR (x); }
/* inserts at front, removing dublicates */
inline SCM ly_assoc_front_x(SCM alist, SCM key, SCM val)
{
  return scm_acons(key, val, scm_assoc_remove_x (alist, key));
}
#ifdef PARANOID
#define gh_pair_p ly_pair_p
bool ly_pair_p (SCM x);
#else
inline bool ly_pair_p (SCM x) { return SCM_NFALSEP (scm_pair_p (x)); }
#endif
inline bool ly_symbol_p (SCM x) { return SCM_SYMBOLP (x); }
inline bool ly_number_p (SCM x) { return SCM_NUMBERP (x); }
inline bool ly_procedure_p (SCM x) { return SCM_NFALSEP (scm_procedure_p(x)); }

/*
  display and print newline.
 */
extern "C" { 
void ly_display_scm (SCM s);
}

#include "array.hh"

void read_lily_scm_file (String);
void init_lily_guile (String dir);

bool ly_dir_p (SCM s);
bool ly_number_pair_p (SCM);
bool ly_axis_p (SCM);

/*
  these conversion functions also do a typecheck on the argument, and
  return a default value if S has the wrong type.
*/

Direction to_dir (SCM s);
bool to_boolean (SCM s);

void init_ly_protection ();
unsigned int ly_scm_hash (SCM s);

SCM index_get_cell (SCM cell, Direction d);
SCM index_set_cell (SCM cell, Direction d, SCM val);



SCM ly_snoc (SCM s, SCM list);
SCM ly_split_list (SCM s, SCM list);
SCM ly_unique (SCM list);
/*
  snarfing.
 */
void add_scm_init_func (void (*) ());


typedef SCM (*Scheme_function_unknown) ();

#if __GNUC__ > 2 || __GNUC_MINOR__ >= 96
typedef SCM (*Scheme_function_0) ();
typedef SCM (*Scheme_function_1) (SCM);
typedef SCM (*Scheme_function_2) (SCM,SCM);	 
typedef SCM (*Scheme_function_3) (SCM,SCM, SCM);	 
#else
typedef SCM (*Scheme_function_0) (...);
typedef SCM (*Scheme_function_1) (...);
typedef SCM (*Scheme_function_2) (...);
typedef SCM (*Scheme_function_3) (...);
#endif

#define DECLARE_SCHEME_CALLBACK(NAME,ARGS) \
	static SCM NAME ARGS; \
	static SCM NAME ## _proc

#define MAKE_SCHEME_CALLBACK(TYPE, FUNC, ARGCOUNT) \
SCM TYPE :: FUNC ## _proc;\
void								\
TYPE ## _ ## FUNC ## _init_functions ()					\
{								\
  TYPE :: FUNC ## _proc = gh_new_procedure ## ARGCOUNT  ## _0 (#TYPE "::" #FUNC, \
 ((Scheme_function_ ## ARGCOUNT)TYPE :: FUNC)); 				\
}								\
								\
ADD_SCM_INIT_FUNC (TYPE ## _ ## FUNC ## _callback, TYPE ## _ ## FUNC ## _init_functions);	\


void ly_add_function_documentation (char const * fname,
				    char const * varlist,
				    char const * doc);

#define ADD_SCM_INIT_FUNC(name, func)\
class name ## _scm_initter {			\
public:\
  name ## _scm_initter ()			\
  {						\
    add_scm_init_func (func);		\
  }						\
} _ ## name ## _scm_initter;			\
/* end define */

#define LY_DEFINE(FNAME, PRIMNAME, REQ, OPT, VAR, ARGLIST, DOCSTRING) \
SCM FNAME ARGLIST ; \
SCM FNAME ## _proc;\
void \
FNAME ## init ()\
{\
 FNAME ## _proc \
    = scm_c_define_gsubr (PRIMNAME,REQ, OPT, VAR, (Scheme_function_unknown) FNAME);\
  ly_add_function_documentation (PRIMNAME, #ARGLIST,  DOCSTRING);\
}\
ADD_SCM_INIT_FUNC (FNAME ## init_unique_prefix, FNAME ## init);\
SCM \
FNAME ARGLIST\





#endif // LILY_GUILE_HH
