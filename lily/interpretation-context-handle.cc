/*   
  interpretation-context-handle.cc --  implement Interpretation_context_handle
  
  source file of the GNU LilyPond music typesetter
  
  (c) 1999--2000 Han-Wen Nienhuys <hanwen@cs.uu.nl>
  
 */

#include "interpretation-context-handle.hh"
#include "translator-group.hh"

void
Interpretation_context_handle::up (Translator_group*t)
{
  report_to_l_ = t;
  t->iterator_count_ ++;
}

void
Interpretation_context_handle::down()
{
  report_to_l_->iterator_count_ --;
  report_to_l_ = 0;
}

bool
Interpretation_context_handle::try_music (Music *m)
{
  return  report_to_l_->try_music (m);
}

Interpretation_context_handle::Interpretation_context_handle (Interpretation_context_handle const&s)
{
  report_to_l_ =0;
  if (s.report_to_l_)
    up (s.report_to_l_ );
}

void
Interpretation_context_handle::operator = (Interpretation_context_handle const &s)
{
  set_translator (s.report_to_l_);
}

void
Interpretation_context_handle::set_translator (Translator_group*trans)
{
  if (report_to_l_ ==trans)
    return;
  if (report_to_l_)
    down ();
  if (trans)
    up (trans);
}

Translator_group*
Interpretation_context_handle::report_to_l ()const
{
  return report_to_l_;
}

Interpretation_context_handle::Interpretation_context_handle()
{
  report_to_l_ =0;
}

Interpretation_context_handle::~Interpretation_context_handle ()
{
  if (report_to_l_)
    down ();
}

