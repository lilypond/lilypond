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
#include "request.hh"
#include "request-iterator.hh"
#include "output-property.hh"

void
Music_iterator::do_print() const
{
}

void
Music_iterator::print() const
{
#ifndef NPRINT
  if (!flower_dstream)
    return ;
  DEBUG_OUT << classname(this) << "{";
  Translator_group *t =     report_to_l();
  DEBUG_OUT << "report to " << t->type_str_ << " = " << t->id_str_ << "\n";
  if (ok())
    DEBUG_OUT << "next at " << next_moment() << " ";
  else
    DEBUG_OUT << "not feeling well today..";
  do_print();
  DEBUG_OUT << "}\n";
#endif
}

Translator_group*
Music_iterator::get_req_translator_l()
{
  assert (report_to_l());
  if (report_to_l()->is_bottom_translator_b ())
    return report_to_l();

  set_translator (report_to_l()->get_default_interpreter ());
  return report_to_l();
}


Translator_group* 
Music_iterator::report_to_l() const
{
  return handle_.report_to_l ();
}


void
Music_iterator::set_translator (Translator_group*trans)
{
  handle_.set_translator (trans);
}

void
Music_iterator::construct_children()
{
}

Music_iterator::~Music_iterator()
{
}

Moment
Music_iterator::next_moment() const
{
  return 0;
}


void
Music_iterator::process_and_next (Moment m)
{
  do_process_and_next (m);
}

void
Music_iterator::do_process_and_next (Moment)
{
  first_b_ = false;
}

bool
Music_iterator::ok() const
{
  return first_b_;
}

Music_iterator*
Music_iterator::static_get_iterator_p (Music const *m)
{
  Music_iterator * p =0;

  /* It would be nice to do this decentrally, but the order of this is
     significant.  */
  if (dynamic_cast<Request_chord  const *> (m))
    p = new Request_chord_iterator;
  else if (dynamic_cast<Lyric_combine_music const*> (m))
    p = new Lyric_combine_music_iterator;
  else if (dynamic_cast<Simultaneous_music  const *> (m)) 
    p =  new Simultaneous_music_iterator;
  else if (dynamic_cast<Sequential_music  const *> (m)) 
    p =  new Sequential_music_iterator;
  else if (dynamic_cast<Translation_property  const *> (m))
    p = new Property_iterator;
  else if (dynamic_cast<Change_translator  const *> (m))
    p = new Change_iterator;
  else if (dynamic_cast<Time_scaled_music  const *> (m))
    p = new Time_scaled_music_iterator;
  else if (dynamic_cast<Grace_music const*> (m))
    p = new Grace_iterator;
  else if (dynamic_cast<Auto_change_music const*> (m))
    p = new Auto_change_iterator;
  else if (dynamic_cast<Music_wrapper  const *> (m))
    p = new Music_wrapper_iterator;
  else if (Repeated_music const * n = dynamic_cast<Repeated_music const *> (m))
    {
      if (n->fold_b_)
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
Music_iterator::init_translator (Music const *m, Translator_group  *report_l)
{
  music_l_ = m;
  if (Context_specced_music const * csm =dynamic_cast<Context_specced_music const*>(m))
    {
      Translator_group* a =report_l->
	find_create_translator_l (csm->translator_type_str_, csm->translator_id_str_);

      set_translator (a);
      
    }

  if (! report_to_l())
    set_translator (report_l);
}


Music_iterator*
Music_iterator::get_iterator_p (Music const*m) const
{
  Music_iterator*p = static_get_iterator_p (m);
  p->init_translator (m, report_to_l());
  
  p->construct_children();
  return p;
}

Music_iterator::Music_iterator()
{
  first_b_ = true;
}

Music_iterator*
Music_iterator::try_music (Music const *m) const
{
  bool b = report_to_l ()->try_music ((Music*)m); // ugh
  Music_iterator * it = b ? (Music_iterator*) this : 0;	// ugh
  if (!it)
    it = try_music_in_children (m);
  return it;
}

Music_iterator*
Music_iterator::try_music_in_children (Music const *  ) const
{
  return 0;
}

