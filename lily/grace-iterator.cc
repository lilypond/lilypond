/*   
  grace-music.cc --  implement Grace_music
  
  source file of the GNU LilyPond music typesetter
  
  (c) 1999--2002 Han-Wen Nienhuys <hanwen@cs.uu.nl>
  
 */

#include "grace-music.hh"
#include "grace-iterator.hh"


#include "global-translator.hh"
#include "warn.hh"


Grace_iterator::~Grace_iterator () 
{
  //  child_iter_p_ = 0;
}


void
Grace_iterator::process (Moment m)
{
  Moment main ;
  main.main_part_ = - start_mom_.grace_part_ + m.grace_part_;
  Music_wrapper_iterator::process (main);
}

void
Grace_iterator::construct_children ()
{
  Music_wrapper_iterator::construct_children ();
}



Moment
Grace_iterator::pending_moment () const
{
  Moment cp =Music_wrapper_iterator::pending_moment();

  Moment pending;
  pending.grace_part_ =  start_mom_.grace_part_ + cp.main_part_;

  return pending;
}


IMPLEMENT_CTOR_CALLBACK (Grace_iterator);
