/*   
  moment.cc --  implement Moment
  
  source file of the GNU LilyPond music typesetter
  
  (c) 1999--2002 Han-Wen Nienhuys <hanwen@cs.uu.nl>
  
 */


#include "lily-guile.hh"
#include "moment.hh"
#include "warn.hh"
#include "ly-smobs.icc"


IMPLEMENT_SIMPLE_SMOBS (Moment);
IMPLEMENT_TYPE_P (Moment, "moment?");

SCM
Moment::mark_smob (SCM)
{
  return SCM_EOL;
}


SCM
Moment::smobbed_copy () const
{
  Moment * m = new Moment (*this);
  return m->smobbed_self ();
}


int
Moment::print_smob (SCM s, SCM port, scm_print_state *)
{
  Moment  *r = (Moment *) ly_cdr (s);
     
  scm_puts ("#<Mom ", port);
  String str (r->str ());
  scm_puts ((char *)str.ch_C (), port);
  scm_puts (" >", port);
  
  return 1;
}

/*
  TODO: add optional factor argument.
 */
LY_DEFINE (make_moment,"make-moment", 2,0,0, (SCM n, SCM d),
	   "create the rational number with main timing @var{n}/@var{d}. 


Moment is a point in musical time. It is consists of a pair of
rationals (@var{m},@var{g}), where @var{m} is the timing for the  main
notes, and @var{g} the timing for  grace notes. In absence of grace
notes, @var{g} is zero.
")
{
  Moment m (Rational (1,1));

  if (SCM_INUMP (n) && SCM_INUMP (d))
    {
      m =  Moment (Rational (gh_scm2int (n), gh_scm2int (d)));
    }
  else
    {
      ::error ("make-moment takes two integer arguments. Using 1/1");
    }

  return m.smobbed_copy ();
}

SCM
Moment::equal_p (SCM a, SCM b)
{
  Moment *m1 = unsmob_moment (a);
  Moment *m2 = unsmob_moment (b);
      
  return (*m1 == *m2) ? SCM_BOOL_T : SCM_BOOL_F;
}

/****************************************************************/

int
compare (Moment const &a, Moment const &b)
{
  return Moment::compare (a,b);
}

int
Moment::compare (Moment const &a, Moment const &b)
{
  int c = Rational::compare (a.main_part_,b.main_part_);
  if (c)
    return c;

  return Rational::compare (a.grace_part_, b.grace_part_);
}

Moment::Moment ()
{

}

Moment::Moment (int m)
{
  main_part_ = Rational(m);
  grace_part_  = Rational( 0);
}

Moment::Moment (Rational m, Rational g)
{
  main_part_ = m;
  grace_part_  = g;
}

Moment::Moment (Rational m)
{
  main_part_ = m;
  grace_part_  = Rational (0);
}

void
Moment::operator += (Moment const &src)
{
  main_part_ +=src.main_part_ ;
  grace_part_ += src.grace_part_;
}
void
Moment::operator -= (Moment const &src)
{
  main_part_ -= src.main_part_ ;
  grace_part_ -= src.grace_part_;
}

/*
  only take the main part of SRC for multiplication.
 */
void
Moment::operator *= (Moment const &src)
{
  main_part_ *= src.main_part_ ;
  grace_part_ *= src.main_part_;
}

/*
  only take the main part of SRC for multiplication.
 */
void
Moment::operator /= (Moment const &src)
{
  main_part_ /= src.main_part_ ;
  grace_part_ /= src.main_part_;
}


#if 0
Moment::operator Rational()
{
  return main_part_;
}
#endif

int
Moment::den () const { return main_part_.den (); }

int
Moment::num () const { return main_part_.num (); }

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


String
Moment::str () const
{
  String s =  main_part_.str ();
  if (grace_part_)
    {
      s += "G" + grace_part_.str ();
    }
  return s;
}

Moment
Moment::operator - () const
{
  Moment m;
  m.grace_part_ = - grace_part_;
  m. main_part_ = - main_part_ ;
  return m;
}


#ifdef STREAM_SUPPORT
ostream &
operator << (ostream &os, Moment const &m)
{
  os << m.str ();
  return os;
}
#endif
