/*
  music-iterator.cc -- implement Music_iterator

  source file of the GNU LilyPond music typesetter

  (c)  1997--1998 Han-Wen Nienhuys <hanwen@cs.uu.nl>
*/
#include "debug.hh"
#include "music-list.hh"
#include "music-iterator.hh"
#include "property-iterator.hh"
#include "request-iterator.hh"
#include "sequential-music-iterator.hh"
#include "simultaneous-music-iterator.hh"
#include "translator-group.hh"
#include "translation-property.hh"
#include "change-iterator.hh"
#include "change-translator.hh"
#include "music-wrapper.hh"
#include "music-wrapper-iterator.hh"
#include "compressed-music-iterator.hh"
#include "compressed-music.hh"
#include "repeated-music.hh"
#include "repeated-music-iterator.hh"

void
Music_iterator::do_print() const
{
}

void
Music_iterator::print() const
{
#ifndef NPRINT
  if (!check_debug)
    return ;
  DOUT << classname(this) << "{";
  DOUT << "report to " << 
    report_to_l() << " (" << classname (report_to_l ()) << ")\n";
  if (ok())
    DOUT << "next at " << next_moment() << " ";
  else
    DOUT << "not feeling well today..";
  do_print();
  DOUT << "}\n";
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
Music_iterator::static_get_iterator_p (Music const *m, Translator_group *report_l)
{
  Music_iterator * p =0;
  if (dynamic_cast<Request_chord  const *> (m))
    p = new Request_chord_iterator;
  else if (dynamic_cast<Simultaneous_music  const *> (m)) 
    p =  new Simultaneous_music_iterator;
  else if (dynamic_cast<Sequential_music  const *> (m)) 
    p =  new Sequential_music_iterator;
  else if (dynamic_cast<Translation_property  const *> (m))
    p = new Property_iterator;
  else if (dynamic_cast<Change_translator  const *> (m))
    p = new Change_iterator;
  else if (dynamic_cast<Compressed_music  const *> (m))
    p = new Compressed_music_iterator;
  else if (dynamic_cast<Music_wrapper  const *> (m))
    p = new Music_wrapper_iterator;
  else if (dynamic_cast<Repeated_music const *> (m))
    p = new Repeated_music_iterator;
  else
    assert (0);

  p->music_l_ = m;
  if (m->translator_type_str_.length_i ())
    {
      Translator_group* a =report_l->
	find_create_translator_l (m->translator_type_str_, m->translator_id_str_);
      p->set_translator (a);
    }

  if (! p->report_to_l())
    p ->set_translator (report_l);

  
  return p;
}


Music_iterator*
Music_iterator::get_iterator_p (Music const*m) const
{
  Music_iterator*p = static_get_iterator_p (m, report_to_l());
  p->construct_children();
  return p;
}

Music_iterator::Music_iterator()
{
  first_b_ = true;
}


