/*
  object-key.cc --  implement Object_key

  source file of the GNU LilyPond music typesetter

  (c) 2004 Han-Wen Nienhuys <hanwen@xs4all.nl>

*/


#include "object-key.hh"
#include "ly-smobs.icc"

SCM
Object_key::mark_smob (SCM key)
{
  Object_key* k = (Object_key*) SCM_CELL_WORD_1 (key);
  k->derived_mark();
  return SCM_EOL;
}

void
Object_key::derived_mark () const
{
  
}

Object_key::~Object_key()
{
}

int
Object_key::get_type () const
{
  return GENERAL_KEY;
}

int
Object_key::print_smob (SCM smob, SCM port, scm_print_state*)
{
  return 1;
}

Object_key::Object_key ()
{
  smobify_self ();
}

int
Object_key::compare (Object_key const *other) const
{
  if (this == other)
    return 0;
  
  int c = sign (get_type () -  other->get_type());
  if (c)
    return c;
  else
    return do_compare (other);
}

IMPLEMENT_SMOBS (Object_key);

SCM
Object_key::equal_p (SCM a , SCM b) 
{
  Object_key *ka = unsmob_key (a);
  Object_key *kb = unsmob_key (b);
  
  return (ka->compare (kb)) ? SCM_BOOL_F : SCM_BOOL_T;
}

int
Object_key::do_compare (Object_key const *other) const
{
  return 0;
}

/****************************************************************/

Copied_key::Copied_key (Object_key const* key, int count)
{
  copy_count_ = count;
  original_ = key;
}

int
Copied_key::get_type () const
{
  return COPIED_KEY;
}

int
Copied_key::do_compare (Object_key const *key) const
{
  Copied_key const *other = dynamic_cast<Copied_key const*> (key);
  
  int c = original_->compare (other->original_);
  if (c)
    return c;

  return sign (copy_count_ - other->copy_count_);
}

void
Copied_key::derived_mark () const
{
  scm_gc_mark (original_->self_scm ());
}
