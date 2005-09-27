/*
  paper-system.cc -- implement Paper_system

  source file of the GNU LilyPond music typesetter

  (c) 2004--2005 Jan Nieuwenhuizen <janneke@gnu.org>
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
  stencil_ = s;
  staff_refpoints_ = Interval (0, 0);
  init_vars ();
}


Paper_system::~Paper_system ()
{
}

SCM
Paper_system::mark_smob (SCM smob)
{
  Paper_system *system = (Paper_system *) SCM_CELL_WORD_1 (smob);
  scm_gc_mark (system->mutable_property_alist_);
  return system->stencil_.expr ();
}

int
Paper_system::print_smob (SCM smob, SCM port, scm_print_state*)
{
  Paper_system *p = (Paper_system *) SCM_CELL_WORD_1 (smob);
  scm_puts ("#<", port);
  scm_puts (classname (p), port);
  scm_display (p->mutable_property_alist_, port);
  
  scm_puts (" >", port);
  return 1;
}


Stencil
Paper_system::to_stencil () const
{
  return stencil_;
}

void
Paper_system::init_vars ()
{
  SCM yext = get_property ("Y-extent");
  SCM staff_ext = get_property ("refpoint-Y-extent");

  if (scm_is_pair (yext)
      && is_number_pair (scm_cdr (yext)))
    {
      Box b = stencil_.extent_box();
      b[Y_AXIS] = ly_scm2interval (scm_cdr (yext));
      
      stencil_ = Stencil (b, stencil_.expr ());
    }

  if (scm_is_pair (staff_ext)
      && is_number_pair (scm_cdr (staff_ext)))
    {
      staff_refpoints_ = ly_scm2interval (scm_cdr (staff_ext));
    }
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

/*
  todo: move to Paper_system property.
 */
Interval
Paper_system::staff_refpoints () const
{
  return staff_refpoints_;
}
