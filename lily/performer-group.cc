/*
  performer-group-performer.cc -- implement Performer_group

  source file of the GNU LilyPond music typesetter

  (c) 1996--2009 Han-Wen Nienhuys <hanwen@xs4all.nl>
  Jan Nieuwenhuizen <janneke@gnu.org>
*/

#include "performer-group.hh"

#include "context.hh"
#include "audio-element.hh"
#include "warn.hh"

ADD_TRANSLATOR_GROUP (Performer_group,
		      /* doc */
		      "",

		      /* create */
		      "",

		      /* read */
		      "",

		      /* write */
		      ""
		      );

void
Performer_group::announce_element (Audio_element_info info)
{
  announce_infos_.push_back (info);
  Translator_group *t
    = context ()->get_parent_context ()->implementation ();

  if (Performer_group *eg = dynamic_cast<Performer_group *> (t))
    eg->announce_element (info);
}

void
Performer_group::acknowledge_audio_elements ()
{
  for (vsize j = 0; j < announce_infos_.size (); j++)
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
Performer_group::do_announces ()
{
  for (SCM s = context ()->children_contexts ();
       scm_is_pair (s); s = scm_cdr (s))
    {
      Context *c = unsmob_context (scm_car (s));
      Performer_group *group
	= dynamic_cast<Performer_group *> (c->implementation ());
      if (group)
	group->do_announces ();
    }

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
