/*
  music-iterator.cc -- implement Music_iterator

  source file of the GNU LilyPond music typesetter

  (c)  1997--2000 Han-Wen Nienhuys <hanwen@cs.uu.nl>
*/

/*
  UGH. too many includes.
 */
#include "debug.hh"
#include "music-iterator.hh"
#include "translator-group.hh"
#include "music-wrapper.hh"
#include "music-wrapper-iterator.hh"
#include "simple-music-iterator.hh"
#include "context-specced-music.hh"

Music_iterator::Music_iterator ()
{
  //  clone_i_ = 0;
}

Music_iterator::Music_iterator (Music_iterator const& src)
{
  //  clone_i_ = src.clone_i_ + 1;
  handle_ = *src.handle_.clone ();
  music_l_ = src.music_l_;
  music_length_ = src.music_length_;
}

Music_iterator::~Music_iterator ()
{
}




Translator_group* 
Music_iterator::report_to_l () const
{
  return handle_.report_to_l ();
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
Music_iterator::skip (Moment )
{
}

SCM
Music_iterator::get_music (Moment)const
{
  return SCM_EOL;
}

Music_iterator*
Music_iterator::static_get_iterator_p (Music *m)
{
  Music_iterator * p =0;

  SCM type = m->get_mus_property ("type") ;
  if (unsmob_cxx_function (type))
    {
      Cxx_function f =  unsmob_cxx_function (type);
      
      p = (Music_iterator*) (*f) (SCM_EOL);
    }
  else if (dynamic_cast<Music_wrapper   *> (m))
    p = new Music_wrapper_iterator;
  else
    {
      p = new Simple_music_iterator ;
    }

  p->music_l_ = m;
  p->music_length_ = m->length_mom ();
  
  return p;
}


Moment
Music_iterator::music_length_mom() const
{
  return music_length_;
}

void
Music_iterator::init_translator (Music *m, Translator_group *report_l)
{
  music_l_ = m;
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
	=report_l->find_create_translator_l (c_type, c_id);

      set_translator (a);
      
    }

  if (! report_to_l ())
    set_translator (report_l);
}


Music_iterator*
Music_iterator::get_iterator_p (Music *m) const
{
  Music_iterator*p = static_get_iterator_p (m);
  p->init_translator (m, report_to_l ());
  
  p->construct_children ();
  return p;
}

Music_iterator*
Music_iterator::try_music (Music *m) const
{
  bool b = report_to_l ()->try_music ( (Music*)m); // ugh
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

IMPLEMENT_CTOR_CALLBACK(Music_iterator);
