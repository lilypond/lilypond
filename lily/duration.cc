/*
  duration.cc -- implement Duration
  
  source file of the LilyPond music typesetter

  (c)  1997--2002 Jan Nieuwenhuizen <janneke@gnu.org>
           Han-Wen Nienhuys <hanwen@cs.uu.nl>

*/

#include <assert.h>

#include "misc.hh"
#include "lily-proto.hh"
#include "string.hh"
#include "moment.hh"
#include "duration.hh"
#include "ly-smobs.icc"



int
Duration::compare (Duration const &left, Duration const &right)
{
  return Rational::compare (left.get_length (), right.get_length ());
}

Duration::Duration ()
{
  durlog_ = 0;
  dots_ = 0;
  factor_ = Rational (1,1);
}

Duration::Duration (int l, int d)
{
  durlog_ = l;
  dots_ = d;
  factor_ = Rational (1,1);
}

Duration
Duration::compressed (Rational m) const
{
  Duration d (*this);
  d.factor_ *= m;
  return d;
}

Rational
Duration::get_length () const
{
  Rational mom (1 << abs (durlog_));

  if (durlog_> 0)
    mom = Rational (1)/mom;

  Rational delta = mom;

  for (int d = dots_; d; d--)
    {
      delta /= Rational (2);
      mom += delta;
    }

  return mom * factor_;
}



String
Duration::string () const
{
  String s;

  if (durlog_ < 0  )
    s = "log = "  + to_string (durlog_);
  else
    s = to_string (1 << durlog_);
  
  s += to_string ('.', dots_);
  if (factor_ != Moment (Rational (1,1)))
    {
      s += "*" + factor_.string ();
    }
  return s;
}


IMPLEMENT_TYPE_P (Duration, "duration?");

SCM
Duration::mark_smob (SCM)
{
  return SCM_EOL;
}

IMPLEMENT_SIMPLE_SMOBS (Duration);
int
Duration::print_smob (SCM s, SCM port, scm_print_state *)
{
  Duration  *r = (Duration *) ly_cdr (s);
     
  scm_puts ("#<Duration ", port);
  scm_display (scm_makfrom0str (r->string ().to_str0 ()), port);
  scm_puts (" >", port);
  
  return 1;
}

SCM
Duration::equal_p (SCM a , SCM b)
{
  Duration  *p = (Duration *) ly_cdr (a);
  Duration  *q = (Duration *) ly_cdr (b);  

  bool eq = p->dots_ == q->dots_
    && p->durlog_ == q->durlog_
    && p->factor_ == q->factor_;

  return eq ? SCM_BOOL_T : SCM_BOOL_F;
}
  
MAKE_SCHEME_CALLBACK (Duration, less_p, 2);
SCM
Duration::less_p (SCM p1, SCM p2)
{
  Duration *a = unsmob_duration (p1);
  Duration *b = unsmob_duration (p2);

  if (compare (*a, *b) < 0)
    return SCM_BOOL_T;
  else
    return SCM_BOOL_F;
}

LY_DEFINE(make_duration,
	  "make-duration", 2, 2, 0, (SCM length, SCM dotcount,
				     SCM num, SCM den),
	  "
@var{length} is the negative logarithm (base 2) of the duration:
1 is a half note, 2 is a quarter note, 3 is an eighth
note, etc.  The number of dots after the note is given by
@var{dotcount}.

The duration factor is optionally given by @var{num} and @var{den}.

A duration is a musical duration, i.e. a length of time described by a
power of two (whole, half, quarter, etc.) and a number of augmentation
dots. 

")
{
  SCM_ASSERT_TYPE(gh_number_p (length), length, SCM_ARG1, __FUNCTION__, "integer");
  SCM_ASSERT_TYPE(gh_number_p (dotcount), dotcount, SCM_ARG2, __FUNCTION__, "integer");

  bool compress = false;
  if (num != SCM_UNDEFINED)
    {
      SCM_ASSERT_TYPE(gh_number_p (num), length, SCM_ARG3, __FUNCTION__, "integer");
      compress = true;
    }
  else
    num = gh_int2scm (1);
  
  if (den != SCM_UNDEFINED)
    SCM_ASSERT_TYPE(gh_number_p (den), length, SCM_ARG4, __FUNCTION__, "integer");
  else
    den = gh_int2scm (1);
  
  Duration p (gh_scm2int (length), gh_scm2int (dotcount));
  if (compress)
    p = p.compressed (Rational (gh_scm2int (num), gh_scm2int (den)));

  return p.smobbed_copy ();
}



LY_DEFINE(duration_log,
	  "duration-log", 1, 0, 0, (SCM dur),
	  "
Extract the duration log from @var{dur}"
)
{
  SCM_ASSERT_TYPE(unsmob_duration(dur), dur, SCM_ARG1, __FUNCTION__, "duration");

  return gh_int2scm (unsmob_duration (dur)->duration_log ());
}


LY_DEFINE(dot_count_log,
	  "duration-dot-count", 1, 0, 0, (SCM dur),
	  "
Extract the dot count from @var{dur}"
)
{
  SCM_ASSERT_TYPE(unsmob_duration(dur), dur, SCM_ARG1, __FUNCTION__, "duration");

  return gh_int2scm (unsmob_duration (dur)->dot_count ());
}


LY_DEFINE(ly_intlog2,
	  "intlog2", 1, 0, 0, (SCM d),
	  "
Extract the dot count from @var{dur}"
)
{
  SCM_ASSERT_TYPE(gh_number_p (d), d, SCM_ARG1, __FUNCTION__, "integer");

  int l = intlog2 (gh_scm2int (d));

  return gh_int2scm (l);
}

LY_DEFINE(compression_factor,
	  "duration-factor", 1, 0, 0, (SCM dur),
	  "
Extract the compression factor from @var{dur}. Return as a pair."
)
{
  SCM_ASSERT_TYPE(unsmob_duration(dur), dur, SCM_ARG1, __FUNCTION__, "duration");
  Rational r =unsmob_duration (dur)->factor ();

  return gh_cons(gh_int2scm (r.num()),gh_int2scm (r.den ())); 
}

SCM
Duration::smobbed_copy ()const
{
  Duration *  p = new Duration (*this);
  return p->smobbed_self ();
}

int
Duration::duration_log () const
{
  return durlog_;
}

int
Duration::dot_count () const
{
  return dots_;
}

