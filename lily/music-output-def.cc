/*
  music-output-def.cc -- implement Music_output_def

  source file of the GNU LilyPond music typesetter

  (c)  1997--1998 Han-Wen Nienhuys <hanwen@cs.uu.nl>
*/

#include "scope.hh"
#include "debug.hh"
#include "music-output-def.hh"
#include "global-translator.hh"
#include "dictionary-iter.hh"
#include "identifier.hh"

int
Music_output_def::get_next_default_count () const
{
  return 0;
}

IMPLEMENT_IS_TYPE_B(Music_output_def);

Music_output_def::Music_output_def ()
{
  scope_p_ = new Scope;
  translator_p_dict_p_ = new Scope;
}

Music_output_def::~Music_output_def ()
{
  delete scope_p_;
  delete translator_p_dict_p_;
}

Music_output_def::Music_output_def (Music_output_def const &s)
{
  scope_p_ = new Scope (*s.scope_p_);
  translator_p_dict_p_ = new Scope (*s.translator_p_dict_p_);
  
  for (Dictionary_iter<Identifier*> i (*translator_p_dict_p_);  i.ok (); i++)
    {
      Translator * t = i.val ()->access_Translator (false);
      t-> output_def_l_ = this;
    }
}

void
Music_output_def::assign_translator (Translator*tp)
{
  String s =tp->type_str_;
  if (translator_p_dict_p_->elem_b (s))
    delete translator_p_dict_p_->elem (s);
  
  (*translator_p_dict_p_)[s] = new Translator_identifier (tp, 0);
  tp ->output_def_l_ = this;
}

Translator*
Music_output_def::find_translator_l (String name) const
{
  if (translator_p_dict_p_->elem_b (name))
    return (*translator_p_dict_p_)[name]->access_Translator ();

  if (global_translator_dict_p->elem_b (name))
    return (*global_translator_dict_p)[name];

  return 0;
}


Global_translator *
Music_output_def::get_global_translator_p () 
{
  Translator * t = find_translator_l ("Score");
  if (!t)
    error (_("Can't find Score context"));
  t = t->clone ();
  Global_translator *g = t->access_Translator_group ()->global_l ();
  t->add_processing ();
  
  return g;
}

void
Music_output_def::print () const
{
#ifndef NPRINT
  translator_p_dict_p_->print ();
  scope_p_->print( );
#endif
}

String
Music_output_def::get_default_output () const
{
  if (!scope_p_->elem_b ("output"))
    return "";  
  Identifier * id = (*scope_p_) ["output"];

  String *p = id->access_String ();
  return p ? *p : String ("");
}
