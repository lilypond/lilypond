/*
  lilypond-key.cc --  implement Lilypond_{grob,context}_key

  source file of the GNU LilyPond music typesetter

  (c) 2004 Han-Wen Nienhuys <hanwen@xs4all.nl>

*/


#include "object-key.hh"
#include "lilypond-key.hh"

Lilypond_grob_key::Lilypond_grob_key (Object_key const *context,
				      Moment start,
				      String name,
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
  scm_gc_mark (context_->self_scm ());
}

int
Lilypond_grob_key::do_compare (Object_key const* key) const
{
  Lilypond_grob_key const * other = dynamic_cast<Lilypond_grob_key const*> (key); 
  int c;

  c = context_->compare (other->context_);
  if (c)
    return c;
  
  c = Moment::compare (creation_moment_, other->creation_moment_);
  if (c)
    return c;

  c = String::compare (grob_name_, other->grob_name_);
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

/****************************************************************/


void
Lilypond_context_key::derived_mark () const
{
  if (parent_context_)
    scm_gc_mark (parent_context_->self_scm ());
}

Lilypond_context_key::Lilypond_context_key (Object_key const *parent,
					    Moment start,
					    String type,
					    String id,
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
  Lilypond_context_key const * other
    = dynamic_cast<Lilypond_context_key const*> (key); 

  int c;
  if (parent_context_)
    {
      c = parent_context_->compare (other->parent_context_);
      if (c)
	return c;
    }
  
  c = Moment::compare (start_moment_, other->start_moment_);
  if (c)
    return c;

  c = String::compare (context_name_, other->context_name_);
  if (c)
    return c;

  c = String::compare (id_, other->id_);
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
