/*
  music-output-def.cc -- implement Music_output_def

  source file of the GNU LilyPond music typesetter

  (c)  1997--2002 Han-Wen Nienhuys <hanwen@cs.uu.nl>
*/

#include "scm-hash.hh"

#include "dictionary.hh"
#include "warn.hh"
#include "music-output-def.hh"
#include "global-translator.hh"
#include "translator-def.hh"
#include "main.hh"
#include "file-path.hh"
#include "lily-guile.hh"

#include "ly-smobs.icc"

int
Music_output_def::get_next_score_count () const
{
  return 0;
}

Music_output_def::Music_output_def ()
{
  style_sheet_ = SCM_EOL;
  scaled_fonts_ = SCM_EOL;

  variable_tab_ = new Scheme_hash_table;
  translator_tab_ = new Scheme_hash_table;

  smobify_self ();
  scm_gc_unprotect_object (variable_tab_->self_scm ());
  scm_gc_unprotect_object (translator_tab_->self_scm ());  
}

Music_output_def::~Music_output_def ()
{
}

Music_output_def::Music_output_def (Music_output_def const &s)
{
  variable_tab_ = new Scheme_hash_table (*s.variable_tab_);
  translator_tab_ = new Scheme_hash_table (*s.translator_tab_);

  style_sheet_ = SCM_EOL;
  scaled_fonts_ = SCM_EOL;

  smobify_self ();
  scm_gc_unprotect_object (variable_tab_->self_scm ());
  scm_gc_unprotect_object (translator_tab_->self_scm ());  
  
  style_sheet_ = scm_list_copy (s.style_sheet_);
  scaled_fonts_ = scm_list_copy (s.scaled_fonts_);  
}


IMPLEMENT_SMOBS (Music_output_def);

IMPLEMENT_DEFAULT_EQUAL_P (Music_output_def);

SCM
Music_output_def::mark_smob (SCM m)
{
  Music_output_def * mo = (Music_output_def*) SCM_CELL_WORD_1 (m);
  scm_gc_mark (mo->style_sheet_);
  scm_gc_mark (mo->translator_tab_->self_scm ());
  scm_gc_mark (mo->variable_tab_->self_scm ());

  return mo->scaled_fonts_;
}

void
Music_output_def::assign_translator (SCM transdef)
{
  Translator_def *tp = unsmob_translator_def (transdef);
  assert (tp);

  
  SCM s = ly_symbol2scm ("");
  if (gh_string_p (tp->type_name_))
    s = scm_string_to_symbol (tp->type_name_);

  translator_tab_->set (s, transdef);
}

/*
  find the translator for NAME. NAME may be a string or a symbol.
 */
SCM
Music_output_def::find_translator (SCM name) const
{  
  if (gh_string_p (name))
    name = scm_string_to_symbol (name);
  
  SCM val  =SCM_EOL;
  translator_tab_->try_retrieve (name, &val);
  return val;
}


Global_translator *
Music_output_def::get_global_translator () 
{
  SCM key = ly_symbol2scm ("Score");
  Translator_def * t = unsmob_translator_def (find_translator (key));

  if (!t)
    error (_f ("can't find `%s' context", "Score"));

  Translator_group * tg = t->instantiate (this);
  
  tg->initialize ();
  
  return dynamic_cast <Global_translator *> (tg);
}

int
Music_output_def::print_smob (SCM s, SCM p, scm_print_state *)
{
  Music_output_def * def = unsmob_music_output_def (s);
  scm_puts ("#<Music_output_def ", p);
#if 0  
  scm_write (def->type_name_, p);
#else  
  (void) def;
#endif  
  scm_puts (">", p);
  return 1;
}

/*
  ugh: should move into Music_output_def (complication: .midi and .tex
  need separate counts.)  */
String
Music_output_def::outname_string () 
{
  String out = output_name_global;
  int def = get_next_score_count ();
  if (def && out != "-")
    {
      Path p = split_path (out);
      p.base += "-" + to_string (def);
      out = p.string ();
    }
  return out;
}
