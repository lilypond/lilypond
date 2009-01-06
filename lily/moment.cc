/*
  moment.cc -- implement Moment

  source file of the GNU LilyPond music typesetter

  (c) 1999--2009 Han-Wen Nienhuys <hanwen@xs4all.nl>
*/

#include "moment.hh"

#include "warn.hh"

Moment::Moment ()
{
}

Moment::Moment (int m)
{
  main_part_ = Rational (m);
  grace_part_ = Rational (0);
}

Moment::Moment (Rational m, Rational g)
{
  main_part_ = m;
  grace_part_ = g;
}

Moment::Moment (Rational m)
{
  main_part_ = m;
  grace_part_ = Rational (0);
}

#include "ly-smobs.icc"

IMPLEMENT_SIMPLE_SMOBS (Moment);
IMPLEMENT_TYPE_P (Moment, "ly:moment?");

SCM
Moment::mark_smob (SCM)
{
  return SCM_EOL;
}

int
Moment::print_smob (SCM s, SCM port, scm_print_state *)
{
  Moment *r = (Moment *) SCM_CELL_WORD_1 (s);

  scm_puts ("#<Mom ", port);
  string str = r->to_string ();
  scm_puts ((char *)str.c_str (), port);
  scm_puts (">", port);

  return 1;
}

SCM
Moment::as_scheme () const
{
  return scm_list_5 (ly_symbol2scm ("ly:make-moment"),
		     scm_from_int64 (main_part_.num ()),
		     scm_from_int64 (main_part_.den ()),
		     scm_from_int64 (grace_part_.num ()),
		     scm_from_int64 (grace_part_.den ()));
}

SCM
Moment::equal_p (SCM a, SCM b)
{
  Moment *m1 = unsmob_moment (a);
  Moment *m2 = unsmob_moment (b);

  return (*m1 == *m2) ? SCM_BOOL_T : SCM_BOOL_F;
}

int
compare (Moment const &a, Moment const &b)
{
  return Moment::compare (a, b);
}

int
Moment::compare (Moment const &a, Moment const &b)
{
  int c = Rational::compare (a.main_part_, b.main_part_);
  if (c)
    return c;

  return Rational::compare (a.grace_part_, b.grace_part_);
}

void
Moment::operator += (Moment const &src)
{
  main_part_ += src.main_part_;
  grace_part_ += src.grace_part_;
}

void
Moment::operator -= (Moment const &src)
{
  main_part_ -= src.main_part_;
  grace_part_ -= src.grace_part_;
}

/* Only take the main part of SRC for multiplication.  */
void
Moment::operator *= (Moment const &src)
{
  main_part_ *= src.main_part_;
  grace_part_ *= src.main_part_;
}

/* Only take the main part of SRC for division.  */
void
Moment::operator /= (Moment const &src)
{
  main_part_ /= src.main_part_;
  grace_part_ /= src.main_part_;
}

/* Only take the main part of SRC for division.  */
void
Moment::operator %= (Moment const &src)
{
  main_part_ %= src.main_part_;
  grace_part_ %= src.main_part_;
}

I64
Moment::den () const
{
  /* TODO: ensure MSB == 0 here */
  return main_part_.den ();
}

I64
Moment::num () const
{
  return main_part_.num ();
}

bool
Moment::to_bool () const
{
  return main_part_ || grace_part_;
}

void
Moment::set_infinite (int k)
{
  main_part_.set_infinite (k);
}

string
Moment::to_string () const
{
  string s = main_part_.to_string ();
  if (grace_part_)
    s += "G" + grace_part_.to_string ();
  return s;
}

Moment
Moment::operator - () const
{
  Moment m;
  m.grace_part_ = -grace_part_;
  m.main_part_ = -main_part_;
  return m;
}

#ifdef STREAM_SUPPORT
ostream &
operator << (ostream &os, Moment const &m)
{
  os << m.to_string ();
  return os;
}
#endif

Moment
robust_scm2moment (SCM m, Moment d)
{
  Moment *p = unsmob_moment (m);
  if (!p)
    return d;
  else
    return *p;
}

bool
moment_less (SCM a, SCM b)
{
  return *unsmob_moment (a) < *unsmob_moment (b);
}

