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
Music_output_def::assign_translator (Translator_group*tp)
{
  String s =tp->type_str_;
  if (s.empty_b ())
    {
      tp->warning (_("Interpretation context with empty type"));
    }

  SCM tr = tp->self_scm ();
  scm_unprotect_object (tr);
  translator_p_dict_p_->set (s, tr);
}

Translator*
Music_output_def::find_translator_l (String name) const
{
  if (translator_p_dict_p_->elem_b (name))
    return unsmob_translator (translator_p_dict_p_->scm_elem (name));

  map<String, Translator*>::const_iterator ki
    =global_translator_dict_p->find (name);

  if (ki != global_translator_dict_p->end ())
    return (*ki).second ;

  return 0;
}


Global_translator *
Music_output_def::get_global_translator_p () 
{
  Translator * t = find_translator_l ("Score");
  if (!t)
    error (_f ("can't find `%s' context", "Score"));
  t = t->clone ();

  t->output_def_l_ = this;
  Global_translator *g = dynamic_cast <Global_translator *> (t);
  t->add_processing ();
  
  return g;
}

void
Music_output_def::print () const
{
}

String
Music_output_def::get_default_output () const
{
  if (safe_global_b || !scope_p_->elem_b ("output"))
    return "";
  SCM s =  scope_p_->scm_elem ("output");

  
  
  return gh_string_p (s) ? ly_scm2string (s) : String ("");
}


