/*   
  grace-iterator.cc --  implement Grace_iterator
  
  source file of the GNU LilyPond music typesetter
  
  (c) 1999--2000 Han-Wen Nienhuys <hanwen@cs.uu.nl>
  
 */

#include "grace-iterator.hh"
#include "global-translator.hh"
#include "warn.hh"

Grace_iterator::~Grace_iterator () 
{
  //  child_iter_p_ = 0;
}

void
Grace_iterator::construct_children () 
{
  Translator_group * t = report_to_l ()->find_create_translator_l ("Grace", ""); // umgh.

  if (t)
    set_translator (t);
  Music_wrapper_iterator::construct_children ();
}

void
Grace_iterator::process (Moment)
{
  Global_translator * t = dynamic_cast<Global_translator*>(report_to_l ());
  if (t)
    {
      t->start ();
      t->run_iterator_on_me (child_iter_p_);
      delete child_iter_p_;
      child_iter_p_ = 0;
      t->finish ();
    }
  else
    {
      warning (_("no Grace context available")); 
    }
}

Moment
Grace_iterator::pending_moment () const
{
  return 0;
}


IMPLEMENT_CTOR_CALLBACK(Grace_iterator);
