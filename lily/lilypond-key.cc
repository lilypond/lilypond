/*
  lilypond-key.cc -- implement Lilypond_{grob, context}_key

  source file of the GNU LilyPond music typesetter

  (c) 2004--2007 Han-Wen Nienhuys <hanwen@xs4all.nl>
*/

#include "lilypond-key.hh"

Lilypond_grob_key::Lilypond_grob_key (Object_key const *context,
				      Moment start,
				      string name,
				      int disambiguation_count)
{
  context_ = context;
  creation_moment_ = start;
  grob_name_ = name;
  disambiguation_count_ = disambiguation_count;
}

void
Lilypond_grob_key::derived_mark () const
{
  if (context_)
    scm_gc_mark (context_->self_scm ());
}

// todo: reverse order of comparison for efficiency reasons.
int
Lilypond_grob_key::do_compare (Object_key const *key) const
{
  Lilypond_grob_key const *other = dynamic_cast<Lilypond_grob_key const *> (key);
  int c;

  c = context_->compare (other->context_);
  if (c)
    return c;

  c = Moment::compare (creation_moment_, other->creation_moment_);
  if (c)
    return c;

  c = grob_name_.compare (other->grob_name_);
  if (c)
    return c;

  c = sign (disambiguation_count_ - other->disambiguation_count_);
  if (c)
    return c;

  return 0;
}

int
Lilypond_grob_key::get_type () const
{
  return GROB_KEY;
}

SCM
Lilypond_grob_key::as_scheme () const
{
  return scm_list_4 (context_ ? context_->self_scm () : SCM_BOOL_F,
		     creation_moment_.smobbed_copy (),
		     scm_makfrom0str (grob_name_.c_str ()),
		     scm_from_int (disambiguation_count_));
}

Object_key *
Lilypond_grob_key::from_scheme (SCM a)
{
  return new Lilypond_grob_key (unsmob_key (scm_car (a)),
				*unsmob_moment (scm_cadr (a)),
				ly_scm2string (scm_caddr (a)),
				scm_to_int (scm_list_ref (a, scm_from_int (3))));
}

/****************************************************************/

void
Lilypond_context_key::derived_mark () const
{
  if (parent_context_)
    scm_gc_mark (parent_context_->self_scm ());
}

Lilypond_context_key::Lilypond_context_key (Object_key const *parent,
					    Moment start,
					    string type,
					    string id,
					    int count)
{
  disambiguation_count_ = count;
  parent_context_ = parent;
  start_moment_ = start;
  context_name_ = type;
  id_ = id;
}

int
Lilypond_context_key::do_compare (Object_key const *key) const
{
  Lilypond_context_key const *other
    = dynamic_cast<Lilypond_context_key const *> (key);

  int c;
  if (parent_context_ && other->parent_context_)
    {
      c = parent_context_->compare (other->parent_context_);
      if (c)
	return c;
    }
  else if (parent_context_)
    return -1;
  else if (other->parent_context_)
    return 1;

  c = Moment::compare (start_moment_, other->start_moment_);
  if (c)
    return c;

  c = context_name_.compare (other->context_name_);
  if (c)
    return c;

  c = id_.compare (other->id_);
  if (c)
    return c;

  c = sign (disambiguation_count_ - other->disambiguation_count_);
  if (c)
    return c;

  return 0;
}

int
Lilypond_context_key::get_type () const
{
  return CONTEXT_KEY;
}

SCM
Lilypond_context_key::as_scheme () const
{
  return scm_list_5 (parent_context_ ? parent_context_->self_scm () : SCM_BOOL_F,
		     start_moment_.smobbed_copy (),
		     scm_makfrom0str (context_name_.c_str ()),
		     scm_makfrom0str (id_.c_str ()),
		     scm_from_int (disambiguation_count_));
}

Object_key *
Lilypond_context_key::from_scheme (SCM a)
{
  return new Lilypond_context_key (unsmob_key (scm_car (a)),
				   *unsmob_moment (scm_cadr (a)),
				   ly_scm2string (scm_list_ref (a, scm_from_int (2))),
				   ly_scm2string (scm_list_ref (a, scm_from_int (3))),
				   scm_to_int (scm_list_ref (a, scm_from_int (4))));
}

/****************************************************************/

int
Lilypond_general_key::get_type () const
{
  return GENERAL_KEY;
}

void
Lilypond_general_key::derived_mark () const
{
  if (parent_)
    scm_gc_mark (parent_->self_scm ());
}

Lilypond_general_key::Lilypond_general_key (Object_key const *parent,
					    string name,
					    int count)
{
  parent_ = parent;
  name_ = name;
  disambiguation_count_ = count;
}

int
Lilypond_general_key::do_compare (Object_key const *key)const
{
  Lilypond_general_key const *other
    = dynamic_cast<Lilypond_general_key const *> (key);

  if (parent_ && other->parent_)
    parent_->compare (other->parent_);
  else if (parent_)
    return -1;
  else if (other->parent_)
    return 1;

  int c = name_.compare (other->name_);
  if (c)
    return c;

  c = sign (disambiguation_count_ - other->disambiguation_count_);
  if (c)
    return c;

  return 0;
}

SCM
Lilypond_general_key::as_scheme () const
{
  return scm_list_3 (parent_ ? parent_->self_scm () : SCM_BOOL_F,
		     scm_makfrom0str (name_.c_str ()),
		     scm_from_int (disambiguation_count_));
}

Object_key *
Lilypond_general_key::from_scheme (SCM a)
{
  return new Lilypond_general_key (unsmob_key (scm_car (a)),
				   ly_scm2string (scm_list_ref (a, scm_from_int (1))),
				   scm_to_int (scm_list_ref (a, scm_from_int (2))));
}
