/*   
  interpretation-context-handle.cc --  implement Interpretation_context_handle
  
  source file of the GNU LilyPond music typesetter
  
  (c) 1999--2002 Han-Wen Nienhuys <hanwen@cs.uu.nl>
 */

#include "interpretation-context-handle.hh"
#include "translator-group.hh"

Interpretation_context_handle::Interpretation_context_handle ()
{
  report_to_ =0;
}

Interpretation_context_handle::Interpretation_context_handle (Interpretation_context_handle const&s)
{
  report_to_ =0;
  if (s.report_to_)
    up (s.report_to_);
}

Interpretation_context_handle*
Interpretation_context_handle::clone () const
{
  Interpretation_context_handle* handle = new Interpretation_context_handle;
  handle->report_to_ = this->report_to_;
  return handle;
}

Interpretation_context_handle::~Interpretation_context_handle ()
{
  if (report_to_)
    down ();
}

void
Interpretation_context_handle::up (Translator_group*t)
{
  report_to_ = t;
  t->iterator_count_ ++;
}

void
Interpretation_context_handle::down ()
{
  report_to_->iterator_count_ --;
  report_to_ = 0;
}

bool
Interpretation_context_handle::try_music (Music *m)
{
  return  report_to_->try_music (m);
}

void
Interpretation_context_handle::operator = (Interpretation_context_handle const &s)
{
  set_translator (s.report_to_);
}

void
Interpretation_context_handle::set_translator (Translator_group*trans)
{
  if (report_to_ ==trans)
    return;
  if (report_to_)
    down ();
  if (trans)
    up (trans);
}

Translator_group*
Interpretation_context_handle::report_to ()const
{
  return report_to_;
}


