/*   
  new-grace-music.cc --  implement New-Grace_music
  
  source file of the GNU LilyPond music typesetter
  
  (c) 1999--2001 Han-Wen Nienhuys <hanwen@cs.uu.nl>
  
 */

#include "new-grace-music.hh"
#include "new-grace-iterator.hh"

#include "grace-iterator.hh"
#include "global-translator.hh"
#include "warn.hh"


New_grace_iterator::~New_grace_iterator () 
{
  //  child_iter_p_ = 0;
}


void
New_grace_iterator::process (Moment m )
{
  Moment main ;
  main.main_part_ = m.grace_mom_;
  Music_wrapper_iterator::process (main);
}

void
New_grace_iterator::construct_children ()
{
  Music_wrapper_iterator::construct_children ();
}



Moment
New_grace_iterator::pending_moment () const
{
  Moment cp =Music_wrapper_iterator::pending_moment();

  Moment pending;
  pending.grace_mom_ = - music_length_.main_part_  + cp.main_part_;

  return pending;
}


IMPLEMENT_CTOR_CALLBACK (New_grace_iterator);
