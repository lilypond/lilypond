#include "simple-music-iterator.hh"
#include "context.hh"
#include "music.hh"

/**
  Iterate a property.  
 */
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

  scm_call_1 (proc, get_outlet ()->self_scm ());
  
  Simple_music_iterator::process (m);
}

IMPLEMENT_CTOR_CALLBACK (Apply_context_iterator);
  
