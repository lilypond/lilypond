/*
  translator.cc -- implement Translator

  source file of the GNU LilyPond music typesetter

  (c) 1997--2005 Han-Wen Nienhuys <hanwen@cs.uu.nl>
*/

#include "translator.hh"

#include "warn.hh"
#include "translator-group.hh"
#include "context-def.hh"
#include "global-context.hh"

#include "ly-smobs.icc"

Translator::~Translator ()
{
}

void
Translator::init ()
{
  must_be_last_ = false;
  self_scm_ = SCM_EOL;
  daddy_context_ = 0;
  smobify_self ();
}

void
Translator::do_announces ()
{
}

void
Translator::process_music ()
{
  
}

Translator::Translator ()
{
  init ();
}

Translator::Translator (Translator const &src)
{
  init ();
  must_be_last_ = src.must_be_last_;
}

bool
Translator::try_music (Music *)
{
  return false;
}
			    

Moment
Translator::now_mom () const
{
  return daddy_context_->now_mom ();
}

Output_def *
Translator::get_output_def () const
{
  return daddy_context_->get_output_def ();
}


Translator_group*
Translator::get_daddy_translator () const
{
  return daddy_context_->implementation ();
}


SCM
Translator::internal_get_property (SCM sym) const
{
  return daddy_context_->internal_get_property (sym);
}

void
Translator::stop_translation_timestep ()
{
}

/*
  this function has 2 properties

  - It is called before try_music ()

  - It is called before any user information enters the translators.
  (i.e. any \property or event is not processed yet.)

  */
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
  me->derived_mark ();
  return SCM_EOL;
}

SCM
Translator::translator_description () const
{
  return SCM_EOL;
}


Global_context *
Translator::get_global_context () const
{
  return daddy_context_ ->get_global_context ();
}


Score_context *
Translator::get_score_context () const
{
  return daddy_context_->get_score_context ();
}  


SCM
Translator::static_translator_description ()const
{
  return SCM_EOL;
}


IMPLEMENT_SMOBS (Translator);
IMPLEMENT_DEFAULT_EQUAL_P (Translator);
IMPLEMENT_TYPE_P (Translator,"ly:translator?");

bool
Translator::must_be_last () const
{
  return must_be_last_;
}

void
Translator::derived_mark () const
{
  
}
