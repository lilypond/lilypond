/*   
  musical-pitch.cc --  implement Musical_pitch
  
  source file of the GNU LilyPond music typesetter
  
  (c) 1998 Han-Wen Nienhuys <hanwen@cs.ruu.nl>
  
 */
#include "musical-pitch.hh"
#include "debug.hh"

Musical_pitch::Musical_pitch ()
{
  init ();
}

void
Musical_pitch::init ()
{
  notename_i_ = 0;
  octave_i_ = 0;
  accidental_i_ = 0;
}

void
Musical_pitch::print () const
{
#ifndef NPRINT
  DOUT << str();
#endif
}

int
Musical_pitch::compare (Musical_pitch const &m1, Musical_pitch const &m2)
{
    int o=  m1.octave_i_ - m2.octave_i_;
  int n = m1.notename_i_ - m2.notename_i_;
  int a = m1.accidental_i_ - m2.accidental_i_;

  if (o)
	return o;
  if (n)
	return n;
  if (a)
	return a;
  return 0;
}

int
Musical_pitch::steps () const
{
  return  notename_i_ + octave_i_*7;
}

/*
  should be settable from input to allow "viola"-mode
 */
static Byte pitch_byte_a[  ] = { 0, 2, 4, 5, 7, 9, 11 };

int
Musical_pitch::semitone_pitch () const
{
  return  pitch_byte_a[ notename_i_ % 7 ] + accidental_i_ + octave_i_ * 12;
}

void
Musical_pitch::transpose (Musical_pitch delta)
{
  int old_pitch = semitone_pitch ();
  int delta_pitch = delta.semitone_pitch ();
  octave_i_ += delta.octave_i_;
  notename_i_ += delta.notename_i_;

  
  while  (notename_i_ >= 7)
    {
      notename_i_ -= 7;
      octave_i_ ++;
    }

  int new_pitch = semitone_pitch ();
  int delta_acc = new_pitch - old_pitch - delta_pitch;
  accidental_i_ -= delta_acc;
}


char const *accname[] = {"double flat", "flat", "natural",
			 "sharp" , "double sharp"};

String
Musical_pitch::str () const
{
  int n = (notename_i_ + 2) % 7;
  String s (char(n + 'a'));
  if (accidental_i_)
    s +=   " " + String (accname[accidental_i_ + 2]);

  if (octave_i_)
    s  += String ((octave_i_> 0)? "^": "_") + String(octave_i_);
  

  return s;
}

Musical_pitch
Musical_pitch::to_relative_octave (Musical_pitch p)
{
  int oct_mod = octave_i_  + 1;	// account for c' = octave 1 iso. 0 4
  Musical_pitch up_pitch (p);
  Musical_pitch down_pitch (p);

  up_pitch.accidental_i_ = accidental_i_;
  down_pitch.accidental_i_ = accidental_i_;
  
  up_pitch.up_to (notename_i_);
  down_pitch.down_to (notename_i_);
  int h = p.steps ();
  if (abs (up_pitch.steps () - h) < abs (down_pitch.steps () - h))
    {
      *this =  up_pitch;
      /* this sux imnsho
      if (oct_mod > 0)		// ugh
      oct_mod --;*/
    }
  else
    {
      *this = down_pitch;
      /*      if (oct_mod < 0)
	      oct_mod ++;*/
    }
  
  octave_i_ += oct_mod;
  return *this;
}

void
Musical_pitch::up_to (int notename)
{
  if (notename_i_  > notename)
    {
      octave_i_ ++;
    }
  notename_i_  = notename;
}

void
Musical_pitch::down_to (int notename)
{
  if (notename_i_ < notename)
    {
      octave_i_ --;
    }
  notename_i_ = notename;
}

#if 0

Musical_pitch
My_lily_parser::get_melodic_req (Musical_pitch p, int quotes)
{
  if (relative_octave_mode_b_)
    {
      set_nearest (melodic);
      int d = melodic->pitch () - last_melodic_->pitch ();
      int shift = 0;
      if (quotes && (sign (d) == sign (quotes)))
	shift -= sign (quotes);
      if (!quotes && (abs (d) == 6))
	{
	  String str = _("Octave ambiguity; assuming ");
	  /*
	    [TODO]
	    figure this out.

	    If the distance is exactly*) half an octave, there is 
	    no nearest pitch.  In that case, we'll try to guess what 
	    composer/ typist meant.
	    Firstly, we'll do this by comparing the 'notename distance':
		
	    f b'   % name-distance: f g a b: 3

	    is surely a shorter notename distance than

	    f 'b  % name-distance: b c d e f: 4

	    (should we give a warning at all, or can we safely assume
	    this is a positive interval up?)

	    *) It is conceivable that, musically speaking, the interval
	    with the greater pitch-distance is thought to be smaller?

	  */

	  int name_delta = melodic->notename_i_ - last_melodic_->notename_i_;
	  int name_near = abs (name_delta) % 7;
	  int name_wrap = (7 - abs (name_delta)) % 7;
	  if (name_near != name_wrap)
	    shift = name_near < name_wrap ? sign (name_delta) : -sign (name_delta);
	  else if (sign (last_melodic_->accidental_i_) 
		   != sign (melodic->accidental_i_))
	    shift = last_melodic_->accidental_i_ - melodic->accidental_i_;
	  else
	    shift = -1;
	  String name_str = notename_str (melodic);
	  str += shift > 0 ? name_str + "'" : "'" + name_str;
	  if (sign (d) == sign (shift))
	    shift = 0;
	  melodic->warning (str);
	}
      melodic->octave_i_ += quotes + shift;
    }
  else
    {
      Melodic_req nearest (*melodic);
      set_nearest (&nearest);
      melodic->octave_i_ += quotes;

      if (find_quarts_global_b)
	{
	  int e = melodic->pitch () - nearest.pitch ();
	  if (e)
	    {
	      int d = melodic->pitch () - last_melodic_->pitch ();
	      String str = _("Interval greater than quart");
	      int n = 1 + (abs (d) - 1) / 12;
	      String quote_str ('\'', n);
	      str += _(", relative: ");
	      String name_str = notename_str (melodic);
	      str += d < 0 ? quote_str + name_str : name_str + quote_str;
	      melodic->warning (str);
	    }
	}
    }
  delete last_melodic_;
  last_melodic_ = melodic->clone ()->musical ()->melodic ();
  return melodic;
}
#endif
