/*   
     grace-music.cc --  implement Grace_music
  
     source file of the GNU LilyPond music typesetter
  
     (c) 1999--2004 Han-Wen Nienhuys <hanwen@cs.uu.nl>
  
*/

#include "grace-music.hh"
#include "grace-iterator.hh"

Moment
Grace_music::start_mom () const
{
  Moment *l = unsmob_moment (Music_wrapper::length_callback (self_scm ()));
  Moment gl;
  gl.grace_part_ = -(l->main_part_ + l->grace_part_ );
  return gl;
}

Grace_music::Grace_music (SCM x)
  : Music_wrapper (x)
{
}

ADD_MUSIC (Grace_music);
