/*
  duration-convert.cc -- implement Duration_convert

  source file of the LilyPond music typesetter

  (c)  1997--2000 Han-Wen Nienhuys <hanwen@cs.uu.nl>
	   Jan Nieuwenhuizen <janneke@gnu.org>
*/
#include <assert.h>
#include "duration-convert.hh"
#include "warn.hh"
#include "duration-iter.hh"

Duration_iterator::Duration_iterator ()
{
  cursor_dur_.durlog_i_ = 7;
  if (Duration_convert::no_smaller_than_i_s)
    cursor_dur_.durlog_i_ = Duration_convert::no_smaller_than_i_s;
}

Duration 
Duration_iterator::operator ++(int)
{
  return forward_dur ();
}

Duration
Duration_iterator::dur ()
{
  return cursor_dur_;
}

Duration
Duration_iterator::forward_dur ()
{
  /* should do smart table? guessing: 
     duration wholes
     16 	0.0625
     32.. 	0.0703
     8:2/3	0.0833
     16.	0.0938
     8	0.1250
     16..	0.1406
     4:2/3	0.1667
     8.	0.1875
		
     */
  assert (ok ());

  if (!cursor_dur_.dots_i_ && !cursor_dur_.plet_b ()) 
    {
      cursor_dur_.durlog_i_ += 1;
      cursor_dur_.dots_i_ = 2;
    }
  else if (cursor_dur_.dots_i_ == 2) 
    {
      assert (!cursor_dur_.plet_b ());
      cursor_dur_.dots_i_ = 0;
      cursor_dur_.durlog_i_ -=2;
      cursor_dur_.set_plet (2, 3);
    }
  else if (cursor_dur_.plet_b () 
	   && (cursor_dur_.plet_.iso_i_ == 2)
	   && (cursor_dur_.plet_.type_i_ == 3)) 
    {
      assert (!cursor_dur_.dots_i_);
      cursor_dur_.set_plet (1, 1);
      cursor_dur_.durlog_i_ += 1;
      cursor_dur_.dots_i_ = 1;
    }
  else if (cursor_dur_.dots_i_ == 1) 
    {
      assert (!cursor_dur_.plet_b ());
      cursor_dur_.dots_i_ = 0;
      cursor_dur_.durlog_i_ -= 1;
    }
		
  if (Duration_convert::no_tuplets_b_s
      && cursor_dur_.plet_b () && ok ())
    return forward_dur ();
  if (Duration_convert::no_double_dots_b_s 
      && (cursor_dur_.dots_i_ == 2) && ok ())
    return forward_dur ();
  if (Duration_convert::no_smaller_than_i_s
      && (cursor_dur_.durlog_i_ > Duration_convert::no_smaller_than_i_s) && ok ())
    return forward_dur ();
  if (Duration_convert::no_smaller_than_i_s
      && cursor_dur_.dots_i_
      && (cursor_dur_.durlog_i_ >= Duration_convert::no_smaller_than_i_s)
      && ok ())
    return forward_dur ();
  if (Duration_convert::no_smaller_than_i_s
      && (cursor_dur_.dots_i_ == 2)
      && (cursor_dur_.durlog_i_ >= Duration_convert::no_smaller_than_i_s / 2)
      && ok ())
    return forward_dur ();

  return dur ();
}

bool
Duration_iterator::ok ()
{
  return cursor_dur_.length_mom () <= Rational (4);
}
