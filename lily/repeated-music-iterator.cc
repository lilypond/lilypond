/*   
  repeated-music-iterator.cc --  implement Repeated_music_iterator
  
  source file of the GNU LilyPond music typesetter
  
  (c) 1998 Jan Nieuwenhuizen <janneke@gnu.org>
  
 */

#include "repeated-music-iterator.hh"
#include "repeated-music.hh"
#include "musical-request.hh"
#include "translator-group.hh"
#include "command-request.hh"

Repeated_music_iterator::Repeated_music_iterator ()
{
}

Repeated_music_iterator::~Repeated_music_iterator ()
{
}

void
Repeated_music_iterator::construct_children ()
{
  Music_wrapper_iterator::construct_children ();
  // Sequential_music_iterator::construct_children ();
}

void
Repeated_music_iterator::do_process_and_next (Moment m)
{
  if (Music_wrapper_iterator::ok ())
    Music_wrapper_iterator::do_process_and_next (m);
//  else
    //Sequential_music_iterator::do_process_and_next (m);
}

Music_wrapper*
Repeated_music_iterator::music_wrapper_l () const
{
  return ((Repeated_music*)Music_wrapper_iterator::music_l_)->repeat_p_;
}

bool
Repeated_music_iterator::ok () const
{
  return Music_wrapper_iterator::ok (); // || Sequential_music_iterator:: ok();
}

Sequential_music*
Repeated_music_iterator::sequential_music_l () const
{
  return ((Repeated_music*)Sequential_music_iterator::music_l_)->alternative_p_;
}

void
Repeated_music_iterator::start_next_element ()
{
  // Sequential_music_iterator::start_next_element ();
}

void
Repeated_music_iterator::leave_element ()
{
  // Sequential_music_iterator::leave_element ();
}

