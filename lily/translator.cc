/*
  translator.cc -- implement Translator

  source file of the GNU LilyPond music typesetter

  (c) 1997--2004 Han-Wen Nienhuys <hanwen@cs.uu.nl>
*/


#include "translator.hh"
#include "warn.hh"
#include "translator-group.hh"
#include "context-def.hh"
#include "global-context.hh"
#include "moment.hh"
#include "context.hh"
#include "ly-smobs.icc"


Translator::~Translator ()
{
}

void
Translator::init ()
{
  self_scm_ = SCM_EOL;
  simple_trans_list_ = SCM_BOOL_F;
  daddy_context_ =0;
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

Translator::Translator (Translator const &)
{
  init ();
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
  return me->simple_trans_list_;
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
