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
#include "property-iterator.hh"
#include "request-chord-iterator.hh"
#include "sequential-music-iterator.hh"
#include "simultaneous-music-iterator.hh"
#include "translator-group.hh"
#include "change-iterator.hh"
#include "music-wrapper.hh"
#include "music-wrapper-iterator.hh"
#include "time-scaled-music-iterator.hh"
#include "repeated-music.hh"
#include "folded-repeat-iterator.hh"
#include "unfolded-repeat-iterator.hh"
#include "grace-iterator.hh"
#include "lyric-combine-music-iterator.hh"
#include "auto-change-iterator.hh"
#include "part-combine-music-iterator.hh"
#include "simple-music-iterator.hh"
#include "output-property-music-iterator.hh"
#include "chord-tremolo-iterator.hh"
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



/* We could do this decentrally:

 -  Declare a new smob-type, which stores a function ptr in its CDR
   (and not a struct ptr). The function ptr has signature

   	Music_iterator* (*)()

 - initialize  all music with a set_mus_property("iterator-ctor"),

 - do
 
    func_ptr  p = (func_ptr) gh_cdr (get_mus_property ("iterator-ctor"));
    iter_p = (*p)();

*/

Music_iterator*
Music_iterator::static_get_iterator_p (Music *m)
{
  Music_iterator * p =0;

  SCM type = m->get_mus_property ("type") ;

  if (type == ly_symbol2scm ("property-set"))
    p = new Property_iterator;
  else if (type == ly_symbol2scm ("property-push"))
    p = new Push_property_iterator;
  else if (type == ly_symbol2scm ("property-pop"))
    p = new Pop_property_iterator;
  else if (type == ly_symbol2scm ("output-property"))
    p = new Output_property_music_iterator;
  else if (type == ly_symbol2scm ("request-chord"))
    p = new Request_chord_iterator;
  else  if (type == ly_symbol2scm ("lyric-combine-music"))
    p = new Lyric_combine_music_iterator;
  else if  (type == ly_symbol2scm ("simultaneous-music"))
    p =  new Simultaneous_music_iterator;
  else if (type == ly_symbol2scm ("sequential-music"))
    p =  new Sequential_music_iterator;
  else if (type == ly_symbol2scm ("change-translator"))
    p = new Change_iterator;
  else if (type == ly_symbol2scm ("time-scaled-music"))
    p = new Time_scaled_music_iterator;
  else if (type == ly_symbol2scm ("grace-music"))
    p = new Grace_iterator;
  else if (type == ly_symbol2scm ("auto-change-music"))
    p = new Auto_change_iterator;
  else if (type == ly_symbol2scm ("part-combined-music"))
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

