/*   
  musical-pitch.cc --  implement Pitch
  
  source file of the GNU LilyPond music typesetter
  
  (c) 1998--2001 Han-Wen Nienhuys <hanwen@cs.uu.nl>
  
 */
#include "pitch.hh"
#include "debug.hh"
#include "main.hh"
#include "ly-smobs.icc"



Pitch::Pitch (int o, int n, int a)
{
  notename_i_ = n;
  alteration_i_ = a;
  octave_i_ = o;

  if (n < 0 || n >= 7 ||
      a < -2 || a > 2)
    {
      String s = _ ("Pitch arguments out of range");
      s += ": alteration = " + to_str (a);
      s += ", notename = " + to_str (n);
      warning (s);
    }
  normalise ();
}

Pitch::Pitch ()
{
  notename_i_ = 0;
  alteration_i_ = 0;
  octave_i_ = 0;
}

int
Pitch::compare (Pitch const &m1, Pitch const &m2)
{
  int o=  m1.octave_i_ - m2.octave_i_;
  int n = m1.notename_i_ - m2.notename_i_;
  int a = m1.alteration_i_ - m2.alteration_i_;

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
  return  notename_i_ + octave_i_*7;
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
  int o = octave_i_;
  int n = notename_i_;
  while (n < 0)
    {
      n += 7;
      o --;
    }
  return (o + n / 7) * 12 + pitch_byte_a[n % 7] + alteration_i_;
}

void
Pitch::normalise ()
{
  int pitch = semitone_pitch ();
  while (notename_i_ >= 7)
    {
      notename_i_ -= 7;
      octave_i_++;
      alteration_i_ -= semitone_pitch () - pitch;
    }
  while (notename_i_ < 0)
    {
      notename_i_ += 7;
      octave_i_--;
      alteration_i_ -= semitone_pitch () - pitch;
    }
  while (alteration_i_ >= 3)
    {
      if (notename_i_ == 6)
	{
	  notename_i_ = 0;
	  octave_i_++;
	}
      else
	notename_i_++;

      alteration_i_ = 0;
      alteration_i_ -= semitone_pitch () - pitch;
    }
  while (alteration_i_ <= -3)
    {
      if (notename_i_ == 0)
	{
	  notename_i_ = 6;
	  octave_i_--;
	}
      else
	notename_i_--;

      alteration_i_ = 0;
      alteration_i_ -= semitone_pitch () - pitch;
    }
}

/* WHugh, wat een intervaas */
void
Pitch::transpose (Pitch delta)
{
  int old_semi = semitone_pitch ();
  int delta_semi = delta.semitone_pitch ();
  octave_i_ += delta.octave_i_;
  notename_i_ += delta.notename_i_;

  int new_semi = semitone_pitch ();
  int delta_acc = new_semi - old_semi - delta_semi;
  alteration_i_ -= delta_acc;

  normalise ();
}


/* FIXME
   Merge with *pitch->text* funcs in chord-name.scm
 */
char const *accname[] = {"eses", "es", "", "is" , "isis"};

String
Pitch::str () const
{
  int n = (notename_i_ + 2) % 7;
  String s = to_str (char (n + 'a'));
  if (alteration_i_)
    s += String (accname[alteration_i_ + 2]);

  if (octave_i_ >= 0)
    {
      int o = octave_i_ + 1;
      while (o--)
	s += "'";
    }
  else if (octave_i_ < 0)
    {
      int o = (-octave_i_) - 1;
      while (o--)
	s += to_str (',');
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
  int oct_mod = octave_i_  + 1;	// account for c' = octave 1 iso. 0 4
  Pitch up_pitch (p);
  Pitch down_pitch (p);

  up_pitch.alteration_i_ = alteration_i_;
  down_pitch.alteration_i_ = alteration_i_;
  
  Pitch n = *this;
  up_pitch.up_to (notename_i_);
  down_pitch.down_to (notename_i_);

  int h = p.steps ();
  if (abs (up_pitch.steps () - h) < abs (down_pitch.steps () - h))
    n = up_pitch;
  else
    n = down_pitch;
  
  n.octave_i_ += oct_mod;

  *this = n;
  return *this;
}

void
Pitch::up_to (int notename)
{
  if (notename_i_  > notename)
    {
      octave_i_ ++;
    }
  notename_i_  = notename;
}

void
Pitch::down_to (int notename)
{
  if (notename_i_ < notename)
    {
      octave_i_ --;
    }
  notename_i_ = notename;
}

///MAKE_SCHEME_CALLBACK (Pitch, transpose, 2);
///transpose_proc?
SCM
Pitch::transpose (SCM p, SCM delta)
{
  Pitch t = *unsmob_pitch (p);
  t.transpose (*unsmob_pitch (delta));
  return t.smobbed_copy ();
}

static SCM
pitch_transpose (SCM p, SCM delta)
{
  return Pitch::transpose (p, delta);
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
  scm_display (ly_str02scm (r->str ().ch_C ()), port);
  scm_puts (" >", port);
  
  return 1;
}

SCM
Pitch::equal_p (SCM a , SCM b)
{
  Pitch  *p = (Pitch *) ly_cdr (a);
  Pitch  *q = (Pitch *) ly_cdr (b);  

  bool eq = p->notename_i_ == q->notename_i_
    && p->octave_i_ == q->octave_i_
    && p->alteration_i_ == q->alteration_i_;

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

static SCM
make_pitch (SCM o, SCM n, SCM a)
{
  Pitch p (gh_scm2int (o), gh_scm2int (n), gh_scm2int (a));
  return p.smobbed_copy ();
}

static SCM
pitch_octave (SCM pp)
{
  Pitch *p = unsmob_pitch (pp);
  int q = 0;
  if (!p)
    warning ("Not a pitch");
  else
    q = p->octave_i ();

  return gh_int2scm (q);
}

static SCM
pitch_alteration (SCM pp)
{
  Pitch *p = unsmob_pitch (pp);
  int q = 0;
  if (!p)
    warning ("Not a pitch");
  else
    q = p->alteration_i ();

  return gh_int2scm (q);
}

static SCM
pitch_notename (SCM pp)
{
  Pitch *p = unsmob_pitch (pp);
  int q = 0;
  if (!p)
    warning ("Not a pitch");
  else
    q = p->notename_i ();

  return gh_int2scm (q);
}

static SCM
pitch_semitones (SCM pp)
{
  Pitch *p = unsmob_pitch (pp);
  int q = 0;
  if (!p)
    warning ("Not a pitch");
  else
    q = p->steps ();

  return gh_int2scm (q);
}

static void
add_funcs ()
{
  // should take list?: (make-pitch ' (octave name accidental))
  scm_c_define_gsubr ("make-pitch", 3, 0, 0, (Scheme_function_unknown)make_pitch);

  scm_c_define_gsubr ("pitch-octave", 1, 0, 0, (Scheme_function_unknown)pitch_octave);
  scm_c_define_gsubr ("pitch-notename", 1, 0, 0, (Scheme_function_unknown)pitch_notename);
  scm_c_define_gsubr ("pitch-alteration", 1, 0, 0, (Scheme_function_unknown)pitch_alteration);
  scm_c_define_gsubr ("pitch-semitones", 1, 0, 0, (Scheme_function_unknown)pitch_semitones);
  scm_c_define_gsubr ("Pitch::transpose", 2, 0, 0, (Scheme_function_unknown) pitch_transpose);
}

ADD_SCM_INIT_FUNC (pitch, add_funcs);

SCM
Pitch::smobbed_copy ()const
{
  Pitch *  p = new Pitch (*this);
  return p->smobbed_self ();
}

int
Pitch::octave_i ()const
{
  return octave_i_;
}

int
Pitch::notename_i () const
{
  return notename_i_;
}

int
Pitch::alteration_i () const
{
  return alteration_i_;
}
