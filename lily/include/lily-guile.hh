/*
  lily-guile.hh encapsulate guile

  source file of the GNU LilyPond music typesetter

  (c) 1998--2000 Jan Nieuwenhuizen <janneke@gnu.org>
*/

#ifndef LILY_GUILE_HH
#define LILY_GUILE_HH

#include  "string.hh"

#include <guile/gh.h>
#include <libguile.h>
#include "direction.hh"

#ifndef SCM_PACK
#define SCM_PACK(x) ((SCM) x)
#endif


/*
  conversion functions follow the GUILE naming convention, i.e.

    A ly_B2A (B b);
 */
SCM ly_str02scm (char const*c);
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
void ly_display_scm (SCM s);

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
