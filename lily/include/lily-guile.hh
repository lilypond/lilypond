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

SCM ly_str02scm (char const*c);
SCM ly_eval_str (String s);
SCM ly_symbol2scm (char const *);
String ly_symbol2string (SCM);

SCM ly_offset2scm (Offset o);
Offset ly_scm2offset (SCM s);
SCM ly_eval (SCM a);
SCM ly_parse_scm (char const* s, int* n);
SCM ly_quote_scm (SCM s);
void ly_display_scm (SCM s);
String ly_scm2string (SCM s);

#include "array.hh"

void read_lily_scm_file (String);
void init_lily_guile ();

bool isdir_b (SCM s);
Direction to_dir (SCM s);

bool to_boolean (SCM s);

void init_ly_protection ();
unsigned int ly_scm_hash (SCM s);

SCM index_cell (SCM cellp, Direction d);
SCM index_set_cell (SCM cellp, Direction d, SCM val);

template<class T>SCM array_to_scm (Array<T> arr);
template<class T>void scm_to_array (SCM s, Array<T>* arr);

//URG how templates suck!
SCM to_scm (int i);
int scm_to (SCM s, int* i);

SCM to_scm (Real r);
Real scm_to (SCM s, Real* r);

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
