/*
  translator.cc -- implement Translator

  source file of the GNU LilyPond music typesetter

  (c)  1997--2002 Han-Wen Nienhuys <hanwen@cs.uu.nl>
*/


#include "translator.hh"
#include "debug.hh"
#include "translator-group.hh"
#include "translator-def.hh"

#include "moment.hh"
#include "ly-smobs.icc"


Translator::~Translator ()
{
}

void
Translator::init ()
{
  simple_trans_list_ = SCM_EOL;
  trans_group_list_ = SCM_EOL;
  properties_scm_ = SCM_EOL;
  definition_ = SCM_EOL;
  daddy_trans_l_ =0;
}

Translator::Translator ()
{
  init ();
  output_def_l_ = 0;
  smobify_self ();
}

Translator::Translator (Translator const &s)
{
  init ();
  output_def_l_ = s.output_def_l_;
  type_str_ = s.type_str_;

  smobify_self ();
}

bool
Translator::is_alias_b (String s) const
{
  bool b  = s == type_str_;

  for (SCM a = unsmob_translator_def (definition_)->type_aliases_;
       !b && gh_pair_p (a); a = ly_cdr (a))
    b = b || s == ly_scm2string (ly_car (a));

  return b;
}

bool
Translator::try_music (Music *)
{
  return false;
}
			    

Moment
Translator::now_mom () const
{
  return daddy_trans_l_->now_mom ();
}

void
Translator::removal_processing ()
{
  finalize ();
}

void
Translator::do_announces ()
{
}

Music_output_def *
Translator::output_def_l () const
{
  return output_def_l_;
}

SCM
Translator::internal_get_property (SCM sym) const
{
  return daddy_trans_l_->internal_get_property (sym);
}

void
Translator:: stop_translation_timestep ()
{
}

void
Translator::start_translation_timestep ()
{
}

void
Translator::initialize ()
{
}

void
Translator::finalize ()
{
}


/*

  SMOBS

*/
SCM
Translator::mark_smob (SCM sm)
{
  Translator * me = (Translator*) SCM_CELL_WORD_1 (sm);
  scm_gc_mark (me->simple_trans_list_);
  scm_gc_mark (me->trans_group_list_);
  scm_gc_mark (me->definition_);  
  scm_gc_mark (me->properties_scm_);  

  return me->properties_scm_;
}

LY_DEFINE(ly_translator_name,
	  "ly-translator-name", 1,0,0,  (SCM trans),
	  "Return the type name of the translator @var{trans}.
")
{
  Translator* tr =  unsmob_translator (trans);
  SCM_ASSERT_TYPE(tr, trans, SCM_ARG1, __FUNCTION__, "Context");

  char const* nm = classname (tr);
  return ly_str02scm (nm);
}

LY_DEFINE(ly_translator_description,
	  "ly-translator-description",
	  1,0,0, (SCM me),
	  "Return an alist of properties of  translator @var{me}.")
{
  Translator *tr =unsmob_translator (me);
  SCM_ASSERT_TYPE (tr, me, SCM_ARG1, __FUNCTION__, "Context");

  return tr->translator_description ();
}

SCM
Translator::translator_description () const
{
  return SCM_EOL;
}

int
Translator::print_smob (SCM s, SCM port, scm_print_state *)
{
  Translator *sc = (Translator *) ly_cdr (s);
     
  scm_puts ("#<Translator ", port);
  scm_display (ly_translator_name (s), port);
  scm_display (sc->simple_trans_list_, port);
  /*
    don't try to print properties, that is too much hassle.
   */
  scm_puts (" >", port);
  
  return 1;
}

SCM
Translator::static_translator_description ()const
{
  return SCM_EOL;
}


IMPLEMENT_SMOBS (Translator);
IMPLEMENT_DEFAULT_EQUAL_P (Translator);
