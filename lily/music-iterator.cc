/*
  music-iterator.cc -- implement Music_iterator

  source file of the GNU LilyPond music typesetter

  (c)  1997--1998 Han-Wen Nienhuys <hanwen@cs.uu.nl>
*/
#include "debug.hh"
#include "music-list.hh"
#include "music-iterator.hh"
#include "voice-iterator.hh"
#include "property-iterator.hh"
#include "chord-iterator.hh"
#include "request-iterator.hh"
#include "translator-group.hh"
#include "translation-property.hh"
#include "change-iterator.hh"
#include "change-translator.hh"
#include "music-wrapper.hh"
#include "music-wrapper-iterator.hh"
#include "compressed-music-iterator.hh"
#include "compressed-music.hh"



IMPLEMENT_IS_TYPE_B(Music_iterator);


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
  DOUT << name() << "{";
  DOUT << "report to " << 
    report_to_l() << " (" << report_to_l ()->name () << ")\n";
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

void
Music_iterator::push_translator (Translator_group*t)
{
  report_to_l_arr_.push (t);
  t->iterator_count_ ++;
}

void
Music_iterator::pop_translator()
{
  report_to_l()->iterator_count_ --;
  assert (report_to_l()->iterator_count_ >=0);
  report_to_l_arr_.pop();
}

Translator_group* 
Music_iterator::report_to_l() const
{
  if (! report_to_l_arr_.size())
    return 0;
  return report_to_l_arr_.top();
}


void
Music_iterator::set_translator (Translator_group*trans)
{   
  if (report_to_l()==trans)
    return;
  if (report_to_l())
    pop_translator();
  if (trans)
    push_translator (trans);
}

void
Music_iterator::construct_children()
{

}

Music_iterator::~Music_iterator()
{
  set_translator (0);
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
  if (m->is_type_b (Request_chord::static_name()))
    p = new Request_chord_iterator;
  else if (m->is_type_b (Simultaneous_music::static_name())) 
    p =  new Simultaneous_music_iterator;
  else if (m->is_type_b (Sequential_music::static_name())) 
    p =  new Sequential_music_iterator;
  else if (m->is_type_b (Translation_property::static_name ()))
    p = new Property_iterator;
  else if (m->is_type_b (Change_translator::static_name ()))
    p = new Change_iterator;
  else if (m->is_type_b (Compressed_music::static_name ()))
    p = new Compressed_music_iterator;
  else if (m->is_type_b (Music_wrapper::static_name ()))
    p = new Music_wrapper_iterator;

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

void
Music_iterator::inherit_info(Music_iterator const *)
{
}

Music_iterator*
Music_iterator::get_iterator_p (Music const*m) const
{
  Music_iterator*p = static_get_iterator_p (m, report_to_l());
  p->inherit_info (this);
  p->construct_children();
  return p;
}

Music_iterator::Music_iterator()
{
  first_b_ = true;
}


