/*   
  interpretation-context-handle.cc --  implement Interpretation_context_handle
  
  source file of the GNU LilyPond music typesetter
  
  (c) 1999--2004 Han-Wen Nienhuys <hanwen@cs.uu.nl>
 */

#include "interpretation-context-handle.hh"
#include "context.hh"

Interpretation_context_handle::Interpretation_context_handle ()
{
  outlet_ =0;
}

Interpretation_context_handle::Interpretation_context_handle (Interpretation_context_handle const&s)
{
  outlet_ =0;
  if (s.outlet_)
    up (s.outlet_);
}


Interpretation_context_handle::~Interpretation_context_handle ()
{
  /*
    Don't do

    if (outlet_)
      down ();

    with GC, this is asynchronous.
   */
}

void
Interpretation_context_handle::up (Context *t)
{
  outlet_ = t;
  t->iterator_count_ ++;
}

void
Interpretation_context_handle::down ()
{
  outlet_->iterator_count_ --;
  outlet_ = 0;
}

void
Interpretation_context_handle::quit ()
{
  if (outlet_)
    {
      outlet_->iterator_count_ --;
      outlet_ = 0;
    }
}

bool
Interpretation_context_handle::try_music (Music *m)
{
  return outlet_->try_music (m);
}

void
Interpretation_context_handle::operator = (Interpretation_context_handle const &s)
{
  set_context (s.outlet_);
}

void
Interpretation_context_handle::set_context (Context *trans)
{
  if (outlet_ ==trans)
    return;
  if (outlet_)
    down ();
  if (trans)
    up (trans);
}

Context *
Interpretation_context_handle::get_outlet () const
{
  
  return outlet_;
}

int
Interpretation_context_handle::get_count () const
{
  return outlet_->iterator_count_ ;
}
