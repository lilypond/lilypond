/*
  duration.cc -- implement Duration, Plet, 

  source file of the LilyPond music typesetter

  (c)  1997--2000 Jan Nieuwenhuizen <janneke@gnu.org>
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
  factor_ = Moment (1,1);
}

Duration::Duration (int l, int d)
{
  durlog_i_ = l;
  dots_i_ = d;
  factor_ = Moment (1,1);
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
    mom = Moment (1)/mom;

  Rational delta = mom;

  for (int d = dots_i_; d; d--)
    {
      delta /= Moment (2);
      mom += delta;
    }

  return mom * factor_;
}



String
Duration::str () const
{
  String s =  to_str (durlog_i_) + to_str ('.', dots_i_);
  if (factor_ != Moment (1,1))
    {
      s += "*" + factor_.str ();
    }
  return s;
}


IMPLEMENT_TYPE_P(Duration, "duration?");
IMPLEMENT_UNSMOB(Duration, duration);

SCM
Duration::mark_smob (SCM )
{
  return SCM_EOL;
}

IMPLEMENT_SIMPLE_SMOBS(Duration);


int
Duration::print_smob (SCM s, SCM port, scm_print_state *)
{
  Duration  *r = (Duration *) gh_cdr (s);
     
  scm_puts ("#<Duration ", port);
  scm_display (gh_str02scm ((char*)r->str().ch_C()), port);
  scm_puts (" >", port);
  
  return 1;
}

SCM
Duration::equal_p (SCM a , SCM b)
{
  Duration  *p = (Duration *) gh_cdr (a);
  Duration  *q = (Duration *) gh_cdr (b);  

  bool eq = p->dots_i_ == q->dots_i_
    && p->durlog_i_ == q->durlog_i_
    && p->factor_ == q->factor_;

  return eq ? SCM_BOOL_T : SCM_BOOL_F;
}
  
MAKE_SCHEME_CALLBACK(Duration, less_p, 2);
SCM
Duration::less_p (SCM p1, SCM p2)
{
  Duration *a = unsmob_duration (p1);
  Duration *b = unsmob_duration (p2);

  if (compare(*a, *b) < 0 )
    return SCM_BOOL_T;
  else
    return SCM_BOOL_F;
}


static SCM
make_duration (SCM l, SCM d)
{
  Duration p( gh_scm2int (l), gh_scm2int (d));
  return p.smobbed_copy ();
}

static void
add_funcs()
{
  scm_make_gsubr ("make-duration", 2, 0, 0, (Scheme_function_unknown)make_duration);
}

ADD_SCM_INIT_FUNC(duration, add_funcs);

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

