/*
  music-output-def.cc -- implement Music_output_def

  source file of the GNU LilyPond music typesetter

  (c)  1997--1998 Han-Wen Nienhuys <hanwen@stack.nl>
*/


#include "debug.hh"
#include "music-output-def.hh"
#include "global-translator.hh"
#include "dictionary-iter.hh"

int
Music_output_def::get_next_default_count () const
{
  return 0;
}

IMPLEMENT_IS_TYPE_B(Music_output_def);

Music_output_def::Music_output_def ()
{
}

Music_output_def::~Music_output_def ()
{
  for (Dictionary_iter<Translator*> i (translator_p_dict_); i.ok (); i++)
    delete i.val ();
}

Music_output_def::Music_output_def (Music_output_def const &s)
{
  outfile_str_ = s.outfile_str_;
  for (Dictionary_iter<Translator*> i (s.translator_p_dict_); i.ok (); i++)
    assign_translator (i.key (), i.val ()->clone ());
}

Translator*
Music_output_def::find_translator_l (String name) const
{
  if (translator_p_dict_.elt_b (name))
    return translator_p_dict_[name];

  if (global_translator_dict_p->elt_b (name))
    return (*global_translator_dict_p)[name];

  return 0;
}


Global_translator *
Music_output_def::get_global_translator_p () 
{
  Translator * t = find_translator_l ("Score")->clone ();
  Global_translator *g = t->group_l ()->global_l ();
  t->add_processing ();
  
  return g;
}


void
Music_output_def::assign_translator (String s, Translator*t)
{
  t->type_str_ = s;
  t->output_def_l_ = this;
  
  if (translator_p_dict_.elt_b (s))
    delete translator_p_dict_[s];
  translator_p_dict_[s] = t;
}

void
Music_output_def::print () const
{
#ifndef NPRINT
  for (Dictionary_iter<Translator*> i (translator_p_dict_); i.ok (); i++)
    {
      DOUT << i.key () << " = ";
      i.val ()->print ();
    }
  DOUT << "output: " << outfile_str_;
#endif
}
