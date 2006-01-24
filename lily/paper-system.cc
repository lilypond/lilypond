/*
  paper-system.cc -- implement Paper_system

  source file of the GNU LilyPond music typesetter

  (c) 2004--2006 Jan Nieuwenhuizen <janneke@gnu.org>
*/

#include "paper-system.hh"
#include "item.hh"

#include "ly-smobs.icc"

IMPLEMENT_SMOBS (Paper_system);
IMPLEMENT_TYPE_P (Paper_system, "ly:paper-system?");
IMPLEMENT_DEFAULT_EQUAL_P (Paper_system);

Paper_system::Paper_system (Stencil s, SCM immutable_init)
{
  mutable_property_alist_ = SCM_EOL;
  immutable_property_alist_ = immutable_init;
  smobify_self ();

  SCM yext = get_property ("Y-extent");

  if (is_number_pair (yext))
    {
      
      Box b = s.extent_box();
      b[Y_AXIS] = ly_scm2interval (yext);

      s = Stencil (b, s.expr ());
    }
  set_property ("stencil", s.smobbed_copy ());
}

Paper_system::~Paper_system ()
{
}

SCM
Paper_system::mark_smob (SCM smob)
{
  Paper_system *system = (Paper_system *) SCM_CELL_WORD_1 (smob);
  scm_gc_mark (system->mutable_property_alist_);
  return system->immutable_property_alist_;
}

int
Paper_system::print_smob (SCM smob, SCM port, scm_print_state*)
{
  Paper_system *p = (Paper_system *) SCM_CELL_WORD_1 (smob);
  scm_puts ("#<", port);
  scm_puts ("Paper_system", port);
  scm_display (p->mutable_property_alist_, port);
  scm_display (p->immutable_property_alist_, port);
  
  scm_puts (" >\n", port);
  return 1;
}

void
Paper_system::init_vars ()
{

}

SCM
Paper_system::internal_get_property (SCM sym) const
{
  /*
    TODO: type checking
   */
  SCM s = scm_sloppy_assq (sym, mutable_property_alist_);
  if (s != SCM_BOOL_F)
    return scm_cdr (s);

  s = scm_sloppy_assq (sym, immutable_property_alist_);
  return (s == SCM_BOOL_F) ? SCM_EOL : scm_cdr (s);
}

void
Paper_system::internal_set_property (SCM sym, SCM val) 
{
  mutable_property_alist_ = scm_assq_set_x (mutable_property_alist_, sym, val);
}
