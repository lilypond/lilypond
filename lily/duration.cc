/*
  duration.cc -- implement Duration, Plet, 

  source file of the LilyPond music typesetter

  (c)  1997--2002 Jan Nieuwenhuizen <janneke@gnu.org>
           Han-Wen Nienhuys <hanwen@cs.uu.nl>

*/

#include <assert.h>

#include "lily-proto.hh"
#include "string.hh"
#include "moment.hh"
#include "duration.hh"
#include "ly-smobs.icc"



int
Duration::compare (Duration const &left, Duration const &right)
{
  return Rational::compare (left.length_mom (), right.length_mom ());
}

Duration::Duration ()
{
  durlog_i_ = 0;
  dots_i_ = 0;
  factor_ = Rational (1,1);
}

Duration::Duration (int l, int d)
{
  durlog_i_ = l;
  dots_i_ = d;
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
Duration::length_mom () const
{
  Rational mom (1 << abs (durlog_i_));

  if (durlog_i_> 0)
    mom = Rational (1)/mom;

  Rational delta = mom;

  for (int d = dots_i_; d; d--)
    {
      delta /= Rational (2);
      mom += delta;
    }

  return mom * factor_;
}



String
Duration::str () const
{
  String s;

  if (durlog_i_ < 0  )
    s = "log = "  + to_str (durlog_i_);
  else
    s = to_str (1 << durlog_i_);
  
  s += to_str ('.', dots_i_);
  if (factor_ != Moment (Rational (1,1)))
    {
      s += "*" + factor_.str ();
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
  scm_display (ly_str02scm (r->str ().ch_C ()), port);
  scm_puts (" >", port);
  
  return 1;
}

SCM
Duration::equal_p (SCM a , SCM b)
{
  Duration  *p = (Duration *) ly_cdr (a);
  Duration  *q = (Duration *) ly_cdr (b);  

  bool eq = p->dots_i_ == q->dots_i_
    && p->durlog_i_ == q->durlog_i_
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
	  "make-duration", 2, 0, 0, (SCM length, SCM dotcount),
	  "
@var{length} is the negative logarithm (base 2) of the duration:
1 is a half note, 2 is a quarter note, 3 is an eighth
note, etc.  The number of dots after the note is given by
@var{dotcount}.


A duration is a musical duration, i.e. a length of time described by a
power of two (whole, half, quarter, etc.) and a number of augmentation
dots. 

")
{
  SCM_ASSERT_TYPE(gh_number_p(length), length, SCM_ARG1, __FUNCTION__, "integer");
  SCM_ASSERT_TYPE(gh_number_p(dotcount), dotcount, SCM_ARG2, __FUNCTION__, "integer");
  
  Duration p (gh_scm2int (length), gh_scm2int (dotcount));
  return p.smobbed_copy ();
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
  return durlog_i_;
}

int
Duration::dot_count () const
{
  return dots_i_;
}

