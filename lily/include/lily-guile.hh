/*
  lily-guile.hh encapsulate guile

  source file of the GNU LilyPond music typesetter

  (c) 1998--2001 Jan Nieuwenhuizen <janneke@gnu.org>
*/

#ifndef LILY_GUILE_HH
#define LILY_GUILE_HH

#include <guile/gh.h>
#include <libguile.h>
#include "config.h"

/* Guile 1.3.4 compatibility */
#if GUILE_MINOR_VERSION < 4
#define fix_guile_1_3_4_scm_puts(scm_data, port) scm_puts ((char*)scm_data, port)
#define scm_puts(scm_data, port) fix_guile_1_3_4_scm_puts (scm_data, port)
#endif

/* Guile 1.4.x compatibility */
#if GUILE_MINOR_VERSION < 5

#define scm_t_bits scm_bits_t

#define fix_guile_1_4_gh_scm2newstr(str, lenp) gh_scm2newstr (str, (int*)lenp)
#define gh_scm2newstr(str, lenp) fix_guile_1_4_gh_scm2newstr (str, lenp)

#define fix_guile_1_4_scm_primitive_eval(form) scm_eval_3 (form, 1, SCM_EOL)
#define scm_primitive_eval(form) fix_guile_1_4_scm_primitive_eval (form)

#define scm_c_define_gsubr scm_make_gsubr
#define scm_c_memq scm_sloppy_memq
#define scm_gc_protect_object scm_protect_object
#define scm_gc_unprotect_object scm_unprotect_object
#define scm_list_n scm_listify
#define SCM_STRING_CHARS SCM_CHARS
#define SCM_STRING_LENGTH SCM_LENGTH
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
SCM ly_symbol2scm (char const *);
String ly_scm2string (SCM s);
String ly_symbol2string (SCM);
SCM ly_offset2scm (Offset);
Offset ly_scm2offset (SCM);
SCM ly_assoc_chain (SCM key, SCM achain);
Interval ly_scm2interval (SCM);
SCM ly_interval2scm (Interval);

SCM ly_parse_scm (char const* s, int* n);
SCM ly_quote_scm (SCM s);
SCM ly_type (SCM);
bool type_check_assignment (SCM val, SCM sym,  SCM type_symbol) ;
SCM ly_number2string (SCM s);

/*
  display and print newline.
 */
extern "C" { 
void ly_display_scm (SCM s);
}

#include "array.hh"

void read_lily_scm_file (String);
void init_lily_guile ();

bool isdir_b (SCM s);
bool isaxis_b (SCM s);
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

SCM index_cell (SCM cellp, Direction d);
SCM index_set_cell (SCM cellp, Direction d, SCM val);


/*
  snarfing.
 */
void add_scm_init_func (void (*) ());


typedef SCM (*Scheme_function_unknown) ();

#if __GNUC__ > 2 || __GNUC_MINOR__ >= 96
typedef SCM (*Scheme_function_0) ();
typedef SCM (*Scheme_function_1) (SCM);
typedef SCM (*Scheme_function_2) (SCM,SCM);	 
#else
typedef SCM (*Scheme_function_0) (...);
typedef SCM (*Scheme_function_1) (...);
typedef SCM (*Scheme_function_2) (...);
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


#define ADD_SCM_INIT_FUNC(name, func)\
class name ## _scm_initter {			\
public:\
  name ## _scm_initter ()			\
  {						\
    add_scm_init_func (func);		\
  }						\
} _ ## name ## _scm_initter;			\
/* end define */

#endif // LILY_GUILE_HH
