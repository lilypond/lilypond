/*
  music-iterator.cc -- implement Music_iterator

  source file of the GNU LilyPond music typesetter

  (c)  1997--2000 Han-Wen Nienhuys <hanwen@cs.uu.nl>
*/

/*
  UGH. too many includes.
 */
#include "debug.hh"
#include "music-list.hh"
#include "music-iterator.hh"
#include "property-iterator.hh"
#include "request-chord-iterator.hh"
#include "sequential-music-iterator.hh"
#include "simultaneous-music-iterator.hh"
#include "translator-group.hh"
#include "translation-property.hh"
#include "change-iterator.hh"
#include "change-translator.hh"
#include "music-wrapper.hh"
#include "music-wrapper-iterator.hh"
#include "time-scaled-music-iterator.hh"
#include "time-scaled-music.hh"
#include "context-specced-music.hh"
#include "repeated-music.hh"
#include "folded-repeat-iterator.hh"
#include "unfolded-repeat-iterator.hh"
#include "grace-iterator.hh"
#include "grace-music.hh"
#include "lyric-combine-music.hh"
#include "lyric-combine-music-iterator.hh"
#include "auto-change-music.hh"
#include "auto-change-iterator.hh"
#include "part-combine-music.hh"
#include "part-combine-music-iterator.hh"
#include "request.hh"
#include "simple-music-iterator.hh"
#include "output-property.hh"
#include "chord-tremolo-iterator.hh"

Music_iterator::Music_iterator ()
{
  //  clone_i_ = 0;
}

Music_iterator::Music_iterator (Music_iterator const& src)
{
  //  clone_i_ = src.clone_i_ + 1;
  handle_ = *src.handle_.clone ();
  music_l_ = src.music_l_;
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

SCM
Music_iterator::get_music (Moment)const
{
  return SCM_EOL;
}

Music_iterator*
Music_iterator::static_get_iterator_p (Music *m)
{
  Music_iterator * p =0;

  /* It would be nice to do this decentrally, but the order of this is
     significant.  */
  if (dynamic_cast<Request_chord   *> (m))
    p = new Request_chord_iterator;
  else if (dynamic_cast<Lyric_combine_music *> (m))
    p = new Lyric_combine_music_iterator;
  else if (dynamic_cast<Simultaneous_music *> (m)) 
    p =  new Simultaneous_music_iterator;
  else if (dynamic_cast<Sequential_music *> (m)) 
    p =  new Sequential_music_iterator;
  else if (dynamic_cast<Translation_property *> (m))
    p = new Property_iterator;
  else if (dynamic_cast<Change_translator *> (m))
    p = new Change_iterator;
  else if (dynamic_cast<Push_translation_property*> (m))
    p = new Push_property_iterator;
  else if (dynamic_cast<Pop_translation_property*> (m))
    p = new Pop_property_iterator;
  else if (dynamic_cast<Time_scaled_music *> (m))
    p = new Time_scaled_music_iterator;
  else if (dynamic_cast<Grace_music *> (m))
    p = new Grace_iterator;
  else if (dynamic_cast<Auto_change_music *> (m))
    p = new Auto_change_iterator;
  else if (dynamic_cast<Part_combine_music *> (m))
    p = new Part_combine_music_iterator;
  else if (dynamic_cast<Music_wrapper   *> (m))
    p = new Music_wrapper_iterator;
  else if (Repeated_music  * n = dynamic_cast<Repeated_music  *> (m))
    {
      if (n->type_ == "tremolo")
	p = new Chord_tremolo_iterator;
      else if (n->fold_b_)
	p = new Folded_repeat_iterator;
      else
	p = new Unfolded_repeat_iterator;
    }
  else
    {
      p = new Simple_music_iterator ;
    }

  p->music_l_ = m;
  return p;
}

void
Music_iterator::init_translator (Music *m, Translator_group *report_l)
{
  music_l_ = m;
  if (Context_specced_music * csm =dynamic_cast<Context_specced_music *> (m))
    {
      Translator_group* a =report_l->
	find_create_translator_l (csm->translator_type_str_, csm->translator_id_str_);

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

