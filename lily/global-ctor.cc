/*   
  global-ctor.cc --  implement global constructors
  
  source file of the GNU LilyPond music typesetter
  
  (c) 1999--2001 Han-Wen Nienhuys <hanwen@cs.uu.nl>
  
 */
#include "global-ctor.hh"
#include "array.hh"

static Array<Global_ctor> *ctor_global_static_arr_p_;

void
add_constructor (Global_ctor c)
{
  if (!ctor_global_static_arr_p_)
    ctor_global_static_arr_p_ = new Array<Global_ctor>;
  ctor_global_static_arr_p_->push (c);
}

void
call_constructors ()
{
  for (int i=0; i < ctor_global_static_arr_p_->size (); i++)
    (ctor_global_static_arr_p_->elem (i)) ();
}
