/*
  tweak-registration.cc -- implement Tweak_registry

  source file of the GNU LilyPond music typesetter

  (c) 2004--2007 Han-Wen Nienhuys <hanwen@xs4all.nl>
*/

#include "tweak-registration.hh"

#include "object-key-undumper.hh"
#include "grob.hh"

#include "ly-smobs.icc"

Tweak_registry::Tweak_registry ()
{
  undumper_ = 0;
  smobify_self ();
  undumper_ = new Object_key_undumper ();
  undumper_->unprotect ();
}

Tweak_registry::~Tweak_registry ()
{
}

void
Tweak_registry::clear ()
{
  tweaks_.clear ();
  undumper_ = new Object_key_undumper ();
  undumper_->unprotect ();
}

void
Tweak_registry::insert_tweak_from_file (SCM tweak)
{
  SCM skey = scm_car (tweak);

  assert (scm_is_pair (skey)
	  && scm_car (skey) == ly_symbol2scm ("key"));

  Object_key const *key = undumper_->get_key (scm_to_int (scm_cadr (skey)));

  SCM existing = SCM_EOL;
  Tweak_map::const_iterator prev = tweaks_.find (key);
  if (prev != tweaks_.end ())
    existing = (*prev).second;

  tweaks_[key] = scm_cons (scm_cdr (tweak), existing);
}

void
Tweak_registry::insert_grob_tweak (Grob *g, SCM tweak)
{
  Object_key const *key = g->key ();
  if (tweaks_.find (key) == tweaks_.end ())
    tweaks_[key] = SCM_EOL;

  tweaks_[key] = scm_cons (tweak, tweaks_[key]);
}

void
Tweak_registry::replace_grob_tweak (Grob *g, SCM tweak)
{
  Object_key const *key = g->key ();
  tweaks_[key] = scm_cons (tweak, SCM_EOL);
}

SCM
Tweak_registry::get_tweaks (Grob *g)
{
  Object_key const *key = g->key ();
  if (tweaks_.find (key) == tweaks_.end ())
    return SCM_EOL;
  return tweaks_[key];
}

SCM
Tweak_registry::list_tweaks ()
{
  SCM retval = SCM_EOL;
  for (Tweak_map::const_iterator i (tweaks_.begin ());
       i != tweaks_.end ();
       i++)
    {
      Object_key const *key = (*i).first;
      for (SCM t = (*i).second; scm_is_pair (t); t = scm_cdr (t))
	retval = scm_cons (scm_cons (key->self_scm (), scm_car (t)), retval);
    }

  return retval;
}

SCM
Tweak_registry::mark_smob (SCM smob)
{
  Tweak_registry *me = (Tweak_registry *) SCM_CELL_WORD_1 (smob);

  for (Tweak_map::const_iterator i (me->tweaks_.begin ());
       i != me->tweaks_.end ();
       i++)
    {
      scm_gc_mark ((*i).first->self_scm ());
      scm_gc_mark ((*i).second);
    }

  if (me->undumper_)
    scm_gc_mark (me->undumper_->self_scm ());

  return SCM_EOL;
}

int
Tweak_registry::print_smob (SCM smob, SCM port, scm_print_state*)
{
  (void) smob;			// smother warning.
  scm_puts ("#<Tweak_registry>", port);
  return 1;
}

Object_key_undumper *
Tweak_registry::undumper () const
{
  return undumper_;
}

IMPLEMENT_DEFAULT_EQUAL_P (Tweak_registry);
IMPLEMENT_SMOBS (Tweak_registry);

Tweak_registry *global_registry_;

void
init_global_tweak_registry ()
{
  global_registry_ = new Tweak_registry ();
}
