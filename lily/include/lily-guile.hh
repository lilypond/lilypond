/*
  lily-guile.hh encapsulate guile

  source file of the GNU LilyPond music typesetter

  (c) 1998--1999 Jan Nieuwenhuizen <janneke@gnu.org>
*/

#ifndef LILY_GUILE_HH
#define LILY_GUILE_HH

#include  "string.hh"

#include <guile/gh.h>
#include <libguile.h>
#include "direction.hh"

SCM ly_str02scm (char const*c);
SCM ly_eval_str (char const*c);
SCM ly_symbol2scm (char const *);
String ly_symbol2string (SCM);
SCM ly_set_x (String name , SCM val);

SCM ly_append (SCM a, SCM b);
SCM ly_eval (SCM a);
SCM ly_func_o (char const* name);
SCM ly_parse_scm (char const* s, int* n);
SCM ly_quote_scm (SCM s);
void ly_display_scm (SCM s);
String ly_scm2string (SCM s);
SCM array_to_list (SCM *a , int l);


#include "array.hh"

void read_lily_scm_file (String);
void init_lily_guile ();



void init_ly_protection ();
unsigned int ly_scm_hash (SCM s);

SCM index_cell (SCM cellp, Direction d);


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
