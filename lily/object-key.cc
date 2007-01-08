/*
  object-key.cc -- implement Object_key

  source file of the GNU LilyPond music typesetter

  (c) 2004--2007 Han-Wen Nienhuys <hanwen@xs4all.nl>
*/

#include "lilypond-key.hh"
#include "ly-smobs.icc"

SCM
Object_key::mark_smob (SCM key)
{
  Object_key *k = (Object_key *) SCM_CELL_WORD_1 (key);
  k->derived_mark ();
  return SCM_EOL;
}

void
Object_key::derived_mark () const
{
}

Object_key::~Object_key ()
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
  Object_key *k = (Object_key *) SCM_CELL_WORD_1 (smob);
  scm_puts ("#<Object_key ", port);
  scm_display (scm_from_int (k->get_type ()), port);
  scm_puts (">", port);
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

  int c = sign (get_type () - other->get_type ());
  if (c)
    return c;
  else
    return do_compare (other);
}

IMPLEMENT_SMOBS (Object_key);

SCM
Object_key::equal_p (SCM a, SCM b)
{
  Object_key *ka = unsmob_key (a);
  Object_key *kb = unsmob_key (b);

  return (ka->compare (kb)) ? SCM_BOOL_F : SCM_BOOL_T;
}

int
Object_key::do_compare (Object_key const *) const
{
  return 0;
}

SCM
Object_key::dump () const
{
  return scm_cons (scm_from_int (get_type ()),
		   as_scheme ());
}

SCM
Object_key::as_scheme () const
{
  return SCM_EOL;
}

Object_key *
Object_key::from_scheme (SCM)
{
  return new Object_key ();
}

struct Object_dumper_table_entry
{
  Object_key_type type_;
  Object_key *(*ctor_) (SCM);
};

static Object_dumper_table_entry undumpers[]
= {
  {BASE_KEY, Object_key::from_scheme},
  {COPIED_KEY, Copied_key::from_scheme},
  {GENERAL_KEY, Lilypond_general_key::from_scheme},
  {GROB_KEY, Lilypond_grob_key::from_scheme},
  {CONTEXT_KEY, Lilypond_context_key::from_scheme},
  {KEY_COUNT, 0},
};

Object_key *
Object_key::undump (SCM scm_key)
{
  int t = scm_to_int (scm_car (scm_key));
  assert (t == undumpers[t].type_);
  return (undumpers[t].ctor_) (scm_cdr (scm_key));
}

/****************************************************************/

Copied_key::Copied_key (Object_key const *key, int count)
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
  Copied_key const *other = dynamic_cast<Copied_key const *> (key);

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

SCM
Copied_key::as_scheme () const
{
  return scm_list_2 (original_
		     ? original_->self_scm ()
		     : SCM_BOOL_F, scm_from_int (copy_count_));
}

Object_key *
Copied_key::from_scheme (SCM a)
{
  return new Copied_key (unsmob_key (scm_car (a)),
			 scm_to_int (scm_list_ref (a, scm_from_int (1))));
}

