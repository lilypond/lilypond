/*
  lily-guile.hh encapsulate guile

  source file of the GNU LilyPond music typesetter

  (c) 1998--2000 Jan Nieuwenhuizen <janneke@gnu.org>
*/

#ifndef LILY_GUILE_HH
#define LILY_GUILE_HH

#include <guile/gh.h>
#include <libguile.h>

#define fix_guile_1_3_4_scm_puts(scm_data, port) scm_puts ((char*)scm_data, port)
#define scm_puts(scm_data, port) fix_guile_1_3_4_scm_puts (scm_data, port)

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
SCM ly_str02scm (char const*c);
SCM ly_deep_copy (SCM);
SCM ly_symbol2scm (char const *);
String ly_scm2string (SCM s);
String ly_symbol2string (SCM);
SCM ly_offset2scm (Offset);
Offset ly_scm2offset (SCM);

Interval ly_scm2interval (SCM);
SCM ly_interval2scm (Interval);

SCM ly_parse_scm (char const* s, int* n);
SCM ly_quote_scm (SCM s);
SCM ly_type (SCM);

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
void add_scm_init_func (void (*)());


typedef SCM(*Scheme_function_unknown)();

#if __GNUC_MINOR__ >= 96
typedef SCM(*Scheme_function_1)(SCM);	 
#else
typedef SCM(*Scheme_function_1)(...);
#endif

#define MAKE_SCHEME_CALLBACK(TYPE, FUNC) \
static SCM TYPE ## _ ## FUNC ## _proc;\
void								\
TYPE ## _ ## FUNC ## _init_functions ()					\
{								\
  TYPE ## _ ## FUNC ## _proc = gh_new_procedure1_0 (#TYPE "::" #FUNC, \
  ((Scheme_function_1)TYPE :: FUNC)); 				\
}								\
								\
ADD_SCM_INIT_FUNC(TYPE ## _ ## FUNC ## _callback, TYPE ## _ ## FUNC ## _init_functions);	\


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
