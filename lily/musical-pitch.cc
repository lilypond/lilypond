/*   
  musical-pitch.cc --  implement Musical_pitch
  
  source file of the GNU LilyPond music typesetter
  
  (c) 1998--2000 Han-Wen Nienhuys <hanwen@cs.uu.nl>
  
 */
#include "musical-pitch.hh"
#include "debug.hh"
#include "main.hh"
#include "ly-smobs.icc"

int
compare (Array<Musical_pitch>* left, Array<Musical_pitch>* right)
{
  assert (left);
  assert (right);
  
  if (left->size () == right->size ())
    {
      for (int i = 0; i < left->size (); i++)
	{
	  int r = Musical_pitch::compare ((*left)[i], (*right)[i]);
	  if (r)
	    return r;
	}
    }
  else
    return 1;

  return 0;
}

Musical_pitch::Musical_pitch (int o, int n, int a)
{
  notename_i_ = n;
  alteration_i_ = a;
  octave_i_ = o;

  if (n < 0 || n >= 7 ||
      a < -2 || a > 2)
    {
      String s = _("Pitch arguments out of range");
      s += ": alteration = " + to_str (a);
      s += ", notename = " + to_str (n);
      warning (s);
    }
}

Musical_pitch::Musical_pitch ()
{
  notename_i_ = 0;
  alteration_i_ = 0;
  octave_i_ = 0;
}

int
Musical_pitch::compare (Musical_pitch const &m1, Musical_pitch const &m2)
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
Musical_pitch::steps () const
{
  return  notename_i_ + octave_i_*7;
}

/*
  should be settable from input?
 */
static Byte pitch_byte_a[  ] = { 0, 2, 4, 5, 7, 9, 11 };

int
Musical_pitch::semitone_pitch () const
{
  return  pitch_byte_a[ notename_i_ % 7 ] + alteration_i_ + octave_i_ * 12;
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
  alteration_i_ -= delta_acc;
}


#if 0
// nice test for internationalisation strings
char const *accname[] = {"double flat", "flat", "natural",
			 "sharp" , "double sharp"};
#else
char const *accname[] = {"eses", "es", "", "is" , "isis"};
#endif

String
Musical_pitch::str () const
{
  int n = (notename_i_ + 2) % 7;
  String s = to_str (char(n + 'a'));
  if (alteration_i_)
    s += String (accname[alteration_i_ + 2]);

  if (octave_i_ > 0)
    {
      int o = octave_i_ + 1;
      while (o--)
	s += "'";
    }
  else if (octave_i_ <0)
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
Musical_pitch
Musical_pitch::to_relative_octave (Musical_pitch p)
{
  int oct_mod = octave_i_  + 1;	// account for c' = octave 1 iso. 0 4
  Musical_pitch up_pitch (p);
  Musical_pitch down_pitch (p);

  up_pitch.alteration_i_ = alteration_i_;
  down_pitch.alteration_i_ = alteration_i_;
  
  Musical_pitch n = *this;
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

/****************************************************************/


IMPLEMENT_TYPE_P(Musical_pitch, "pitch?");
IMPLEMENT_UNSMOB(Musical_pitch, pitch);
SCM
Musical_pitch::mark_smob (SCM )
{
  return SCM_EOL;
}

IMPLEMENT_SIMPLE_SMOBS(Musical_pitch);


int
Musical_pitch::print_smob (SCM s, SCM port, scm_print_state *)
{
  Musical_pitch  *r = (Musical_pitch *) gh_cdr (s);
     
  scm_puts ("#<Musical_pitch ", port);
  scm_display (gh_str02scm (r->str().ch_C()), port);
  scm_puts (" >", port);
  
  return 1;
}

SCM
Musical_pitch::equal_p (SCM a , SCM b)
{
  Musical_pitch  *p = (Musical_pitch *) gh_cdr (a);
  Musical_pitch  *q = (Musical_pitch *) gh_cdr (b);  

  bool eq = p->notename_i_ == q->notename_i_
    && p->octave_i_ == q->octave_i_
    && p->alteration_i_ == q->alteration_i_;

  return eq ? SCM_BOOL_T : SCM_BOOL_F;
}

MAKE_SCHEME_CALLBACK(Musical_pitch, less_p, 2);
SCM
Musical_pitch::less_p (SCM p1, SCM p2)
{
  Musical_pitch *a = unsmob_pitch (p1);
  Musical_pitch *b = unsmob_pitch (p2);

  if (compare(*a, *b) < 0 )
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
  Musical_pitch p;
  p.octave_i_ = gh_scm2int (o);    
  p.notename_i_ = gh_scm2int (n);
  p.alteration_i_ = gh_scm2int (a);
  return p.smobbed_copy ();
}

static SCM
pitch_octave (SCM pp)
{
  Musical_pitch *p = unsmob_pitch (pp);
  int q = 0;
  if (!p)
    warning ("Not a pitch");
  else
    q = p->octave_i();

  return gh_int2scm (q);
}

static SCM
pitch_alteration (SCM pp)
{
  Musical_pitch *p = unsmob_pitch (pp);
  int q = 0;
  if (!p)
    warning ("Not a pitch");
  else
    q = p->alteration_i();

  return gh_int2scm (q);
}

static SCM
pitch_notename (SCM pp)
{
  Musical_pitch *p = unsmob_pitch (pp);
  int q = 0;
  if (!p)
    warning ("Not a pitch");
  else
    q = p->notename_i();

  return gh_int2scm (q);
}

static SCM
pitch_semitones (SCM pp)
{
  Musical_pitch *p = unsmob_pitch (pp);
  int q = 0;
  if (!p)
    warning ("Not a pitch");
  else
    q = p->steps();

  return gh_int2scm (q);
}

static void
add_funcs()
{
  scm_make_gsubr ("make-pitch", 3, 0, 0, (Scheme_function_unknown)make_pitch);
  scm_make_gsubr ("pitch-octave", 1, 0, 0, (Scheme_function_unknown)pitch_octave);
  scm_make_gsubr ("pitch-notename", 1, 0, 0, (Scheme_function_unknown)pitch_notename);
  scm_make_gsubr ("pitch-alteration", 1, 0, 0, (Scheme_function_unknown)pitch_alteration);
  scm_make_gsubr ("pitch-semitones", 1, 0, 0, (Scheme_function_unknown)pitch_semitones);
}

ADD_SCM_INIT_FUNC(pitch, add_funcs);

SCM
Musical_pitch::smobbed_copy ()const
{
  Musical_pitch *  p = new Musical_pitch (*this);
  return p->smobbed_self ();
}

int
Musical_pitch::octave_i ()const
{
  return octave_i_;
}

int
Musical_pitch::notename_i () const
{
  return notename_i_;
}

int
Musical_pitch::alteration_i () const
{
  return alteration_i_;
}
