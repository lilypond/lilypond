/*
  musical-pitch.cc --  implement Pitch

  source file of the GNU LilyPond music typesetter

  (c) 1998--2004 Han-Wen Nienhuys <hanwen@cs.uu.nl>
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
  normalise ();
}

/* FIXME: why is octave == 0 and default not middleC ? */
Pitch::Pitch ()
{
  notename_ = 0;
  alteration_ = 0;
  octave_ = 0;
}

int
Pitch::compare (Pitch const &m1, Pitch const &m2)
{
  int o =  m1.octave_ - m2.octave_;
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

/* Should be settable from input?  */
static Byte diatonic_scale_semitones[  ] = { 0, 2, 4, 5, 7, 9, 11 };


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

  if (alteration_ % 2)
    programming_error ("semitone_pitch () called on quarter tone alteration.");

  return ((o + n / 7) * 12
	  + diatonic_scale_semitones[n % 7]
	  + (alteration_ / 2));
}

int
Pitch::quartertone_pitch () const
{
  int o = octave_;
  int n = notename_;
  while (n < 0)
    {
      n += 7;
      o --;
    }

  return ((o + n / 7) * 24
	  + 2 * diatonic_scale_semitones[n % 7]
	  + alteration_);
}

void
Pitch::normalise ()
{
  int pitch = quartertone_pitch ();
  while (notename_ >= 7)
    {
      notename_ -= 7;
      octave_++;
      alteration_ -= quartertone_pitch () - pitch;
    }
  while (notename_ < 0)
    {
      notename_ += 7;
      octave_--;
      alteration_ -= quartertone_pitch () - pitch;
    }
  while (alteration_ > DOUBLE_SHARP)
    {
      if (notename_ == 6)
	{
	  notename_ = 0;
	  octave_++;
	}
      else
	notename_++;

      alteration_ = 0;
      alteration_ -= quartertone_pitch () - pitch;
    }

  while (alteration_ < DOUBLE_FLAT)
    {
      if (notename_ == 0)
	{
	  notename_ = 6;
	  octave_--;
	}
      else
	notename_--;

      alteration_ = 0;
      alteration_ -= quartertone_pitch () - pitch;
    }
}

/* WHugh, wat een intervaas */
void
Pitch::transpose (Pitch delta)
{
  int new_semi = quartertone_pitch ()  +delta.quartertone_pitch ();
  octave_ += delta.octave_;
  notename_ += delta.notename_;
  alteration_ += new_semi - quartertone_pitch ();

  normalise ();
}

Pitch
pitch_interval (Pitch const & from , Pitch const & to )
{
  int sound = to.quartertone_pitch ()  - from.quartertone_pitch ();
  Pitch pt (to.get_octave () - from.get_octave (),
	    to.get_notename () - from.get_notename (),

	    to.get_alteration () - from.get_alteration ());

  return pt.transposed (Pitch (0,0,sound - pt.quartertone_pitch ()));
}


/* FIXME
   Merge with *pitch->text* funcs in chord-name.scm  */
char const *accname[] = {"eses", "eseh", "es", "eh", "",
			 "ih", "is" , "isih",  "isis"};

String
Pitch::to_string () const
{
  int n = (notename_ + 2) % 7;
  String s = ::to_string (char (n + 'a'));
  if (alteration_)
    s += String (accname[alteration_ - DOUBLE_FLAT]);

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
	s += ::to_string (',');
    }

  return s;
}

/* Change me to relative, counting from last pitch p
   return copy of resulting pitch.  */
Pitch
Pitch::to_relative_octave (Pitch p) const
{
  /* account for c' = octave 1 iso. 0 4 */
  int oct_mod = octave_  + 1;
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
  return n;
}

void
Pitch::up_to (int notename)
{
  if (notename_ > notename)
    octave_++;
  notename_  = notename;
}

void
Pitch::down_to (int notename)
{
  if (notename_ < notename)
    octave_--;
  notename_ = notename;
}

LY_DEFINE (ly_pitch_transpose, "ly:pitch-transpose",
	   2, 0, 0, (SCM p, SCM delta),
	   "Transpose @var{p} by the amount @var{delta}, "
	   "where @var{delta} is relative to middle C.")
{
  Pitch* t = unsmob_pitch (p);
  Pitch *d = unsmob_pitch (delta);
  SCM_ASSERT_TYPE (t, p, SCM_ARG1, __FUNCTION__, "pitch");
  SCM_ASSERT_TYPE (d, delta, SCM_ARG1, __FUNCTION__, "pitch");
  return t->transposed (*d).smobbed_copy ();
}

IMPLEMENT_TYPE_P (Pitch, "ly:pitch?");

SCM
Pitch::mark_smob (SCM)
{
  return SCM_EOL;
}

IMPLEMENT_SIMPLE_SMOBS (Pitch);
int
Pitch::print_smob (SCM s, SCM port, scm_print_state *)
{
  Pitch *r = (Pitch *) SCM_CELL_WORD_1 (s);
  scm_puts ("#<Pitch ", port);
  scm_display (scm_makfrom0str (r->to_string ().to_str0 ()), port);
  scm_puts (" >", port);
  return 1;
}

SCM
Pitch::equal_p (SCM a , SCM b)
{
  Pitch *p = (Pitch *) SCM_CELL_WORD_1 (a);
  Pitch *q = (Pitch *) SCM_CELL_WORD_1 (b);

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

/* Should add optional args.  */
LY_DEFINE (ly_make_pitch, "ly:make-pitch",
	   3, 0, 0, (SCM octave, SCM note, SCM alter),
	   "@var{octave} is specified by an integer, "
	   "zero for the octave containing middle C.  "
	   "@var{note} is a number from 0 to 6, "
	   "with 0 corresponding to C and 6 corresponding to B.  "
	   "The @var{alter} is zero for a natural, negative for "
	   "flats, or positive for sharps. ")
{
  SCM_ASSERT_TYPE (scm_integer_p (octave)== SCM_BOOL_T , octave, SCM_ARG1, __FUNCTION__, "integer");
  SCM_ASSERT_TYPE (scm_integer_p (note)== SCM_BOOL_T, note, SCM_ARG2, __FUNCTION__, "integer");
  SCM_ASSERT_TYPE (scm_integer_p (alter)== SCM_BOOL_T, alter, SCM_ARG3, __FUNCTION__, "integer");

  Pitch p (scm_to_int (octave), scm_to_int (note), scm_to_int (alter));
  return p.smobbed_copy ();
}

LY_DEFINE (ly_pitch_steps, "ly:pitch-steps", 1, 0, 0,
	   (SCM p),
	   "Number of steps counted from middle C of the pitch @var{p}.")
{
  Pitch *pp = unsmob_pitch (p);
  SCM_ASSERT_TYPE (pp, p, SCM_ARG1, __FUNCTION__, "Pitch");
  return scm_int2num (pp->steps ());
}

LY_DEFINE (ly_pitch_octave, "ly:pitch-octave",
	   1, 0, 0, (SCM pp),
	   "Extract the octave from pitch @var{p}.")
{
  Pitch *p = unsmob_pitch (pp);
  SCM_ASSERT_TYPE (p, pp, SCM_ARG1, __FUNCTION__, "Pitch");
  int q = p->get_octave ();
  return scm_int2num (q);
}

LY_DEFINE (ly_pitch_alteration, "ly:pitch-alteration",
	   1, 0, 0, (SCM pp),
	   "Extract the alteration from pitch  @var{p}.")
{
  Pitch *p = unsmob_pitch (pp);
  SCM_ASSERT_TYPE (p, pp, SCM_ARG1, __FUNCTION__, "Pitch");
  int q = p->get_alteration ();

  return scm_int2num (q);
}

LY_DEFINE (pitch_notename, "ly:pitch-notename",
	   1, 0, 0, (SCM pp),
	   "Extract the note name from pitch  @var{pp}.")
{
  Pitch *p = unsmob_pitch (pp);
  SCM_ASSERT_TYPE (p, pp, SCM_ARG1, __FUNCTION__, "Pitch");
  int q = p->get_notename ();
  return scm_int2num (q);
}

LY_DEFINE (ly_pitch_quartertones, "ly:pitch-quartertones",
	   1, 0, 0, (SCM pp),
	   "Calculate the number of quarter tones of @var{p} from middle C.")
{
  Pitch *p = unsmob_pitch (pp);
  SCM_ASSERT_TYPE (p, pp, SCM_ARG1, __FUNCTION__, "Pitch");
  int q = p->quartertone_pitch ();
  return scm_int2num (q);
}

LY_DEFINE (ly_pitch_semitones, "ly:pitch-semitones",
	   1, 0, 0, (SCM pp),
	   "calculate the number of semitones of @var{p} from middle C.")
{
  Pitch *p = unsmob_pitch (pp);
  SCM_ASSERT_TYPE (p, pp, SCM_ARG1, __FUNCTION__, "Pitch");
  int q = p->semitone_pitch ();
  return scm_int2num (q);
}

LY_DEFINE (ly_pitch_less_p, "ly:pitch<?",
	   2, 0, 0, (SCM p1, SCM p2),
	   "Is @var{p1} lexicographically smaller than @var{p2}?")
{
  Pitch *a = unsmob_pitch (p1);
  Pitch *b = unsmob_pitch (p2);

  SCM_ASSERT_TYPE (a, p1, SCM_ARG1, __FUNCTION__, "Pitch");
  SCM_ASSERT_TYPE (b, p2, SCM_ARG2, __FUNCTION__, "Pitch");

  if (Pitch::compare (*a, *b) < 0)
    return SCM_BOOL_T;
  else
    return SCM_BOOL_F;
}

LY_DEFINE (ly_pitch_diff, "ly:pitch-diff",
	   2 ,0, 0, (SCM pitch, SCM  root),
	   "Return pitch @var{delta} such that @code{pitch} transposed by "
	   "@var{delta} equals @var{root}" )
{
  Pitch *p = unsmob_pitch (pitch);
  Pitch *r = unsmob_pitch (root);
  SCM_ASSERT_TYPE (p, pitch, SCM_ARG1, __FUNCTION__, "Pitch");
  SCM_ASSERT_TYPE (r, root, SCM_ARG2, __FUNCTION__, "Pitch");

  return pitch_interval (*r, *p).smobbed_copy ();
}

int
Pitch::get_octave () const
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

Pitch
Pitch::transposed (Pitch d) const
{
  Pitch p = *this;
  p.transpose (d);
  return p;
}
