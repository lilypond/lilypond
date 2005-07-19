/*
  performer-group-performer.cc -- implement Performer_group_performer

  source file of the GNU LilyPond music typesetter

  (c) 1996--2005 Han-Wen Nienhuys <hanwen@cs.uu.nl>
  Jan Nieuwenhuizen <janneke@gnu.org>
*/

#include "performer-group-performer.hh"

#include "context.hh"
#include "audio-element.hh"
#include "warn.hh"

ADD_TRANSLATOR_GROUP (Performer_group_performer,
		/* descr */ "",
		/* creats*/ "",
		/* accepts */ "",
		/* acks  */ "",
		/* reads */ "",
		/* write */ "");

void
Performer_group_performer::announce_element (Audio_element_info info)
{
  announce_infos_.push (info);
  Translator_group *t
    = context ()->get_parent_context ()->implementation ();

  if (Performer_group_performer *eg = dynamic_cast<Performer_group_performer *> (t))
    eg->announce_element (info);
}

void
Performer_group_performer::acknowledge_audio_elements ()
{
  for (int j = 0; j < announce_infos_.size (); j++)
    {
      Audio_element_info info = announce_infos_[j];

      for (SCM p = get_simple_trans_list (); scm_is_pair (p); p = scm_cdr (p))
	{
	  Translator *t = unsmob_translator (scm_car (p));
	  Performer *eng = dynamic_cast<Performer *> (t);
	  if (eng && eng != info.origin_trans_)
	    eng->acknowledge_audio_element (info);
	}
    }
}

void
performer_each (SCM list, Performer_method method)
{
  for (SCM p = list; scm_is_pair (p); p = scm_cdr (p))
    {
      Performer *e = dynamic_cast<Performer *> (unsmob_translator (scm_car (p)));
      if (e)
	(e->*method) ();
    }
}

void
Performer_group_performer::do_announces ()
{
  while (1)
    {
      performer_each (get_simple_trans_list (),
		      &Performer::create_audio_elements);

      if (!announce_infos_.size ())
	break;

      acknowledge_audio_elements ();
      announce_infos_.clear ();
    }
}


void
Performer_group_performer::play_element (Audio_element *e)
{
  Context *c = context_->get_parent_context ();
  if (c)
    {
      Performer_group_performer *pgp = dynamic_cast<Performer_group_performer*> (c->implementation ());
      pgp->play_element (e);
    }
}

int
Performer_group_performer::get_tempo () const
{
  Context *c = context_->get_parent_context ();
  if (c)
    {
      Performer_group_performer *pgp = dynamic_cast<Performer_group_performer*> (c->implementation ());
      return pgp->get_tempo ();
    }
  return 60;
}
  
