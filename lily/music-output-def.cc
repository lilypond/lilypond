/*
  music-output-def.cc -- implement Music_output_def

  source file of the GNU LilyPond music typesetter

  (c)  1997--2000 Han-Wen Nienhuys <hanwen@cs.uu.nl>
*/

#include "dictionary.hh"
#include "scope.hh"
#include "debug.hh"
#include "music-output-def.hh"
#include "global-translator.hh"
#include "translator-def.hh"
#include "identifier.hh"
#include "main.hh"
#include "lily-guile.hh"

int
Music_output_def::get_next_default_count () const
{
  return 0;
}



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
}

void
Music_output_def::assign_translator (SCM transdef)
{
  Translator_def *tp = unsmob_translator_def (transdef);
  assert (tp);

  String s = ly_scm2string (tp->type_name_);
  translator_p_dict_p_->set (s, transdef);
}

SCM
Music_output_def::find_translator_l (SCM name) const
{
  String s = ly_scm2string (name);
  
  SCM val  =SCM_EOL;
  if (translator_p_dict_p_->elem_b (s))
    return translator_p_dict_p_->scm_elem (s);
  return val;
}


Global_translator *
Music_output_def::get_global_translator_p () 
{
  Translator_def * t = unsmob_translator_def (find_translator_l (gh_str02scm ("Score")));
  if (!t)
    error (_f ("can't find `%s' context", "Score"));

  Translator_group * tg = t->instantiate (this);
  
  tg->add_processing ();
  
  return dynamic_cast <Global_translator *> (tg);
}



String
Music_output_def::get_default_output () const
{
  if (safe_global_b || !scope_p_->elem_b ("output"))
    return "";
  SCM s =  scope_p_->scm_elem ("output");
  
  return gh_string_p (s) ? ly_scm2string (s) : String ("");
}



