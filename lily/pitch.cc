/*   
  musical-pitch.cc --  implement Pitch
  
  source file of the GNU LilyPond music typesetter
  
  (c) 1998--2002 Han-Wen Nienhuys <hanwen@cs.uu.nl>
  
 */
#include "pitch.hh"
#include "warn.hh"
#include "main.hh"
#include "ly-smobs.icc"



Pitch::Pitch (int o, int n, int a)
{
  notename_ = n;
  alteration_ = a;
  octave_ = o;

  if (n < 0 || n >= 7 ||
      a < -2 || a > 2)
    {
      String s = _ ("Pitch arguments out of range");
      s += ": alteration = " + to_string (a);
      s += ", notename = " + to_string (n);
      warning (s);
    }
  normalise ();
}

Pitch::Pitch ()
{
  notename_ = 0;
  alteration_ = 0;
  octave_ = 0;
}

int
Pitch::compare (Pitch const &m1, Pitch const &m2)
{
  int o=  m1.octave_ - m2.octave_;
  int n = m1.notename_ - m2.notename_;
  int a = m1.alteration_ - m2.alteration_;

  if (o)
	return o;
  if (n)
	return n;
  if (a)
	return a;
  return 0;
}

int
Pitch::steps () const
{
  return  notename_ + octave_*7;
}

/*
  should be settable from input?
 */
static Byte pitch_byte_a[  ] = { 0, 2, 4, 5, 7, 9, 11 };


/* Calculate pitch height in 12th octave steps.  Don't assume
   normalised pitch as this function is used to normalise the pitch.  */
int
Pitch::semitone_pitch () const
{
  int o = octave_;
  int n = notename_;
  while (n < 0)
    {
      n += 7;
      o --;
    }
  return (o + n / 7) * 12 + pitch_byte_a[n % 7] + alteration_;
}

void
Pitch::normalise ()
{
  int pitch = semitone_pitch ();
  while (notename_ >= 7)
    {
      notename_ -= 7;
      octave_++;
      alteration_ -= semitone_pitch () - pitch;
    }
  while (notename_ < 0)
    {
      notename_ += 7;
      octave_--;
      alteration_ -= semitone_pitch () - pitch;
    }
  while (alteration_ >= 3)
    {
      if (notename_ == 6)
	{
	  notename_ = 0;
	  octave_++;
	}
      else
	notename_++;

      alteration_ = 0;
      alteration_ -= semitone_pitch () - pitch;
    }
  while (alteration_ <= -3)
    {
      if (notename_ == 0)
	{
	  notename_ = 6;
	  octave_--;
	}
      else
	notename_--;

      alteration_ = 0;
      alteration_ -= semitone_pitch () - pitch;
    }
}

/* WHugh, wat een intervaas */
void
Pitch::transpose (Pitch delta)
{
  int old_semi = semitone_pitch ();
  int delta_semi = delta.semitone_pitch ();
  octave_ += delta.octave_;
  notename_ += delta.notename_;

  int new_semi = semitone_pitch ();
  int delta_acc = new_semi - old_semi - delta_semi;
  alteration_ -= delta_acc;

  normalise ();
}


/* FIXME
   Merge with *pitch->text* funcs in chord-name.scm
 */
char const *accname[] = {"eses", "es", "", "is" , "isis"};

String
Pitch::string () const
{
  int n = (notename_ + 2) % 7;
  String s = to_string (char (n + 'a'));
  if (alteration_)
    s += String (accname[alteration_ + 2]);

  if (octave_ >= 0)
    {
      int o = octave_ + 1;
      while (o--)
	s += "'";
    }
  else if (octave_ < 0)
    {
      int o = (-octave_) - 1;
      while (o--)
	s += to_string (',');
    }

  return s;
}

/*
  change me to relative, counting from last pitch p
  return copy of resulting pitch
 */
Pitch
Pitch::to_relative_octave (Pitch p)
{
  int oct_mod = octave_  + 1;	// account for c' = octave 1 iso. 0 4
  Pitch up_pitch (p);
  Pitch down_pitch (p);

  up_pitch.alteration_ = alteration_;
  down_pitch.alteration_ = alteration_;
  
  Pitch n = *this;
  up_pitch.up_to (notename_);
  down_pitch.down_to (notename_);

  int h = p.steps ();
  if (abs (up_pitch.steps () - h) < abs (down_pitch.steps () - h))
    n = up_pitch;
  else
    n = down_pitch;
  
  n.octave_ += oct_mod;

  *this = n;
  return *this;
}

void
Pitch::up_to (int notename)
{
  if (notename_  > notename)
    {
      octave_ ++;
    }
  notename_  = notename;
}

void
Pitch::down_to (int notename)
{
  if (notename_ < notename)
    {
      octave_ --;
    }
  notename_ = notename;
}
 
LY_DEFINE(ly_pitch_transpose,
	  "ly-transpose-pitch", 2, 0, 0,
	  (SCM p, SCM delta),
	  "Transpose @var{p} by the amount @var{delta}, where @var{delta} is the
pitch that central C is transposed to.
")
{
  Pitch* t = unsmob_pitch (p);
  Pitch *d = unsmob_pitch (delta);
  SCM_ASSERT_TYPE(t, p, SCM_ARG1, __FUNCTION__, "pitch")  ;
  SCM_ASSERT_TYPE(d, delta, SCM_ARG1, __FUNCTION__, "pitch")  ;

  Pitch tp =*t;
  tp.transpose (*d);
  return tp.smobbed_copy ();
}

/****************************************************************/


IMPLEMENT_TYPE_P (Pitch, "pitch?");

SCM
Pitch::mark_smob (SCM)
{
  return SCM_EOL;
}

IMPLEMENT_SIMPLE_SMOBS (Pitch);
int
Pitch::print_smob (SCM s, SCM port, scm_print_state *)
{
  Pitch  *r = (Pitch *) ly_cdr (s);
     
  scm_puts ("#<Pitch ", port);
  scm_display (scm_makfrom0str (r->string ().to_str0 ()), port);
  scm_puts (" >", port);
  
  return 1;
}

SCM
Pitch::equal_p (SCM a , SCM b)
{
  Pitch  *p = (Pitch *) ly_cdr (a);
  Pitch  *q = (Pitch *) ly_cdr (b);  

  bool eq = p->notename_ == q->notename_
    && p->octave_ == q->octave_
    && p->alteration_ == q->alteration_;

  return eq ? SCM_BOOL_T : SCM_BOOL_F;
}

MAKE_SCHEME_CALLBACK (Pitch, less_p, 2);
SCM
Pitch::less_p (SCM p1, SCM p2)
{
  Pitch *a = unsmob_pitch (p1);
  Pitch *b = unsmob_pitch (p2);

  if (compare (*a, *b) < 0)
    return SCM_BOOL_T;
  else
    return SCM_BOOL_F;
}

/*
  should add optional args
 */

LY_DEFINE(make_pitch, "make-pitch", 3, 0, 0, 
	  (SCM o, SCM n, SCM a),
	  "
@var{octave} is specified by an integer, zero for the octave containing
middle C.  @var{note} is a number from 0 to 6, with 0 corresponding to C
and 6 corresponding to B.  The shift is zero for a natural, negative for
flats, or positive for sharps.

")
{
  SCM_ASSERT_TYPE(gh_number_p (o), o, SCM_ARG1, __FUNCTION__, "number");
  SCM_ASSERT_TYPE(gh_number_p (n), n, SCM_ARG2, __FUNCTION__, "number");
  SCM_ASSERT_TYPE(gh_number_p (a), a, SCM_ARG3, __FUNCTION__, "number");

  Pitch p (gh_scm2int (o), gh_scm2int (n), gh_scm2int (a));
  return p.smobbed_copy ();
}


LY_DEFINE(pitch_octave, "pitch-octave", 1, 0, 0, 
	  (SCM pp),
	  "extract the octave from pitch @var{p}.")
{
  Pitch *p = unsmob_pitch (pp);
   SCM_ASSERT_TYPE(p, pp, SCM_ARG1, __FUNCTION__, "Pitch");
  int q = p->get_octave ();

  return gh_int2scm (q);
}

LY_DEFINE(pitch_alteration, "pitch-alteration", 1, 0, 0, 
	  (SCM pp),
	  "extract the alteration from pitch  @var{p}.")
{
  Pitch *p = unsmob_pitch (pp);
  SCM_ASSERT_TYPE(p, pp, SCM_ARG1, __FUNCTION__, "Pitch");
  int     q = p->get_alteration ();

  return gh_int2scm (q);
}

LY_DEFINE(pitch_notename, "pitch-notename", 1, 0, 0, 
	  (SCM pp),
	  "extract the note name from pitch  @var{pp}.")
{
  Pitch *p = unsmob_pitch (pp);
  SCM_ASSERT_TYPE(p, pp, SCM_ARG1, __FUNCTION__, "Pitch");
  int q  = p->get_notename ();

  return gh_int2scm (q);
}

LY_DEFINE(pitch_semitones,  "pitch-semitones", 1, 0, 0, 
	  (SCM pp),
	  "calculate the number of semitones of @var{p} from central C.")
{
  Pitch *p = unsmob_pitch (pp);
  SCM_ASSERT_TYPE(p, pp, SCM_ARG1, __FUNCTION__, "Pitch");
 
  int q = p->semitone_pitch ();
  
  // Was :
  //
  //int q = p->steps ();
  //
  // As the function is called "pitch_semitones", I assume it was a mistake !
  // Jiba

  return gh_int2scm (q);
}

LY_DEFINE(pitch_less, "pitch<?", 2,0,0, (SCM p1, SCM p2),
	  "Is @var{p1} lower than @var{p2}? This uses lexicographic ordening.")
{
  return Pitch::less_p (ly_car (p1),  ly_car (p2));
}

SCM
Pitch::smobbed_copy ()const
{
  Pitch *  p = new Pitch (*this);
  return p->smobbed_self ();
}

int
Pitch::get_octave ()const
{
  return octave_;
}

int
Pitch::get_notename () const
{
  return notename_;
}

int
Pitch::get_alteration () const
{
  return alteration_;
}

