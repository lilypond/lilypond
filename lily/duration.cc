/*
  duration.cc -- implement Duration

  source file of the LilyPond music typesetter

  (c) 1997--2009 Jan Nieuwenhuizen <janneke@gnu.org>
  Han-Wen Nienhuys <hanwen@xs4all.nl>
*/

#include "duration.hh"

#include "misc.hh"
#include "lily-proto.hh"

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
  factor_ = Rational (1, 1);
}

Duration::Duration (int log, int d)
{
  durlog_ = log;
  dots_ = d;
  factor_ = Rational (1, 1);
}

Duration::Duration (Rational r, bool scale)
{
  factor_ = Rational (1, 1);

  if (r.num () == 0.0)
    {
      durlog_ = 0;
      dots_ = 0;
    }
  else
    {
      /* we want to find the integer k for which 2q/p > 2^k >= q/p.
	 It's simple to check that k' = \floor \log q - \floor \log p
	 satisfies the left inequality and is within a factor of 2 of
	 satistying the right one. Therefore either k = k' or k = k'+1 */

      int p = r.num ();
      int q = r.den ();
      int k = intlog2 (q) - intlog2 (p);
      if ((p << k) < q)
	k++;

      assert ((p << k) >= q && (p << (k-1)) < q);

      /* If we were to write out log (p/q) in base 2, then the position of the
	 first non-zero bit (ie. k in our notation) would be the durlog
	 and the number of consecutive 1s after that bit would be the number of
	 dots */
      p = (p << k) - q;
      dots_ = 0;
      while ((p *= 2) >= q)
	{
	  p -= q;
	  dots_++;
	}

      /* we only go up to 64th notes */
      if (k > 6)
	{
	  durlog_ = 6;
	  dots_ = 0;
	}
      else
	durlog_ = k;

      if (scale || k > 6)
	factor_ = r / get_length ();
    }
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

  if (durlog_ > 0)
    mom = Rational (1) / mom;

  Rational delta = mom;
  for (int i = 0; i < dots_; i++)
    {
      delta /= Rational (2);
      mom += delta;
    }

  return mom * factor_;
}

string
Duration::to_string () const
{
  string s;

  if (durlog_ < 0)
    s = "log = " + ::to_string (durlog_);
  else
    s = ::to_string (1 << durlog_);

  s += ::to_string ('.', dots_);
  if (factor_ != Moment (Rational (1, 1)))
    s += "*" + factor_.to_string ();
  return s;
}

IMPLEMENT_TYPE_P (Duration, "ly:duration?");

SCM
Duration::mark_smob (SCM)
{
  return SCM_EOL;
}

IMPLEMENT_SIMPLE_SMOBS (Duration);
int
Duration::print_smob (SCM s, SCM port, scm_print_state *)
{
  Duration *r = (Duration *) SCM_CELL_WORD_1 (s);

  scm_puts ("#<Duration ", port);
  scm_display (ly_string2scm (r->to_string ()), port);
  scm_puts (" >", port);

  return 1;
}

SCM
Duration::equal_p (SCM a, SCM b)
{
  Duration *p = (Duration *) SCM_CELL_WORD_1 (a);
  Duration *q = (Duration *) SCM_CELL_WORD_1 (b);

  bool eq = p->dots_ == q->dots_
    && p->durlog_ == q->durlog_
    && p->factor_ == q->factor_;

  return eq ? SCM_BOOL_T : SCM_BOOL_F;
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
