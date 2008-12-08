/*
  apply-context-iterator.cc -- implement Apply_context_iterator

  source file of the GNU LilyPond music typesetter

  (c) 2004--2008 Han-Wen Nienhuys <hanwen@xs4all.nl>
*/

#include "context.hh"
#include "input.hh"
#include "international.hh"
#include "music.hh"
#include "simple-music-iterator.hh"

class Apply_context_iterator : public Simple_music_iterator
{
public:
  DECLARE_SCHEME_CALLBACK (constructor, ());
protected:
  virtual void process (Moment);
};

void
Apply_context_iterator::process (Moment m)
{
  SCM proc = get_music ()->get_property ("procedure");

  if (ly_is_procedure (proc))
    scm_call_1 (proc, get_outlet ()->self_scm ());
  else
    get_music ()->origin ()->warning (_ ("\\applycontext argument is not a procedure"));

  Simple_music_iterator::process (m);
}

IMPLEMENT_CTOR_CALLBACK (Apply_context_iterator);

