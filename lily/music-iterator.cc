/*
  music-iterator.cc -- implement Music_iterator

  source file of the GNU LilyPond music typesetter

  (c)  1997--2002 Han-Wen Nienhuys <hanwen@cs.uu.nl>
*/

/*
  UGH. too many includes.
 */
#include "warn.hh"
#include "music-iterator.hh"
#include "translator-group.hh"
#include "music-wrapper.hh"
#include "music-wrapper-iterator.hh"
#include "simple-music-iterator.hh"
#include "context-specced-music.hh"

Music_iterator::Music_iterator ()
{
}

Music_iterator::Music_iterator (Music_iterator const& src)
{
  handle_ = *src.handle_.clone ();
  music_ = src.music_;
  music_length_ = src.music_length_;
  start_mom_ = src.start_mom_;
}

Music_iterator::~Music_iterator ()
{
}




Translator_group* 
Music_iterator::report_to () const
{
  return handle_.report_to ();
}


void
Music_iterator::set_translator (Translator_group *trans)
{
  handle_.set_translator (trans);
}

void
Music_iterator::construct_children ()
{
}

Moment
Music_iterator::pending_moment () const
{
  return 0;
}

void
Music_iterator::process (Moment)
{
}

bool
Music_iterator::ok () const
{
  return false;
}

void
Music_iterator::skip (Moment)
{
}

SCM
Music_iterator::get_pending_events (Moment)const
{
  return SCM_EOL;
}

Music_iterator*
Music_iterator::get_static_get_iterator (Music *m)
{
  Music_iterator * p =0;

  SCM ctor = m->get_mus_property ("iterator-ctor") ;
  if (unsmob_cxx_function (ctor))
    {
      Cxx_function f =  unsmob_cxx_function (ctor);
      
      p = (Music_iterator*) (*f) (SCM_EOL);
    }
  else if (dynamic_cast<Music_wrapper   *> (m))
    p = new Music_wrapper_iterator;
  else
    {
      p = new Simple_music_iterator ;
    }

  p->music_ = m;
  assert (m);
  p->music_length_ = m->length_mom ();
  p->start_mom_ = m->start_mom ();
  return p;
}


Moment
Music_iterator::music_length_mom () const
{
  return music_length_;

}

Moment
Music_iterator::music_start_mom ()const
{
  return start_mom_;
}

void
Music_iterator::init_translator (Music *m, Translator_group *report)
{
  music_ = m;
  assert (m);
  if (Context_specced_music * csm =dynamic_cast<Context_specced_music *> (m))
    {
      SCM ct = csm->get_mus_property ("context-type");
      String c_type;
      if (gh_string_p (ct))
	  c_type =  ly_scm2string (ct);
      
      String c_id;
      SCM ci = csm->get_mus_property ("context-id");
      if (gh_string_p (ci))
	c_id = ly_scm2string (ci);
      
      Translator_group* a
	=report->find_create_translator (c_type, c_id);

      set_translator (a);
      
    }

  if (! report_to ())
    set_translator (report);
}


Music_iterator*
Music_iterator::get_iterator (Music *m) const
{
  Music_iterator*p = get_static_get_iterator (m);
  p->init_translator (m, report_to ());
  
  p->construct_children ();
  return p;
}

/*
  TODO: rename to prevent confusion between Translator::try_music and
  Iterator::try_music
  
 */

Music_iterator*
Music_iterator::try_music (Music *m) const
{
  bool b = report_to ()->try_music ((Music*)m); // ugh
  Music_iterator * it = b ? (Music_iterator*) this : 0;	// ugh
  if (!it)
    it = try_music_in_children (m);
  return it;
}

Music_iterator*
Music_iterator::try_music_in_children (Music *) const
{
  return 0;
}

IMPLEMENT_CTOR_CALLBACK (Music_iterator);

Music *
Music_iterator::get_music () const
{
  return music_;
}
