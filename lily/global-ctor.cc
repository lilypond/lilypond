/*   
  global-ctor.cc --  implement global constructors
  
  source file of the GNU LilyPond music typesetter
  
  (c) 1999--2002 Han-Wen Nienhuys <hanwen@cs.uu.nl>
  
 */
#include "global-ctor.hh"
#include "array.hh"

static Array<Global_ctor> *ctor_global_statics_;

void
add_constructor (Global_ctor c)
{
  if (!ctor_global_statics_)
    ctor_global_statics_ = new Array<Global_ctor>;
  ctor_global_statics_->push (c);
}

void
call_constructors ()
{
  for (int i=0; i < ctor_global_statics_->size (); i++)
    (ctor_global_statics_->elem (i)) ();
}
