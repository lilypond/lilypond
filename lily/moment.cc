/*   
  moment.cc --  implement Moment
  
  source file of the GNU LilyPond music typesetter
  
  (c) 1999--2003 Han-Wen Nienhuys <hanwen@cs.uu.nl>
  
 */


#include "lily-guile.hh"
#include "moment.hh"
#include "warn.hh"
#include "ly-smobs.icc"

IMPLEMENT_SIMPLE_SMOBS (Moment);
IMPLEMENT_TYPE_P (Moment, "ly:moment?");

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
  String str = r->to_string ();
  scm_puts ((char *)str.to_str0 (), port);
  scm_puts (">", port);
  
  return 1;
}

/*
  TODO: add optional factor argument.
*/
LY_DEFINE (make_moment,"ly:make-moment", 2,0,0, (SCM n, SCM d),
	   "create the rational number with main timing @var{n}/@var{d}. \n"
	   "\n"
	   "\n"
	   "Moment is a point in musical time. It is consists of a pair of\n"
	   "rationals (@var{m},@var{g}), where @var{m} is the timing for the  main\n"
	   "notes, and @var{g} the timing for  grace notes. In absence of grace\n"
	   "notes, @var{g} is zero.\n"
	   )
{
  SCM_ASSERT_TYPE(SCM_INUMP (n), n, SCM_ARG1, __FUNCTION__, "integer");
  SCM_ASSERT_TYPE(SCM_INUMP (d), d, SCM_ARG2, __FUNCTION__, "integer");

  return Moment (Rational (gh_scm2int (n), gh_scm2int (d))).smobbed_copy();
}

LY_DEFINE (add_moment,"ly:add-moment", 2,0,0, (SCM a, SCM b),
	   "Add two moments."
	   )
{
  Moment * ma = unsmob_moment (a);
  Moment * mb = unsmob_moment (b);
  SCM_ASSERT_TYPE (ma, a, SCM_ARG1, __FUNCTION__, "moment");
  SCM_ASSERT_TYPE (mb, b, SCM_ARG2, __FUNCTION__, "moment");

  return (*ma + *mb).smobbed_copy();
}


LY_DEFINE (mul_moment,"ly:mul-moment", 2,0,0, (SCM a, SCM b),
	   "Multiply two moments."
	   )
{
  Moment * ma = unsmob_moment (a);
  Moment * mb = unsmob_moment (b);
  SCM_ASSERT_TYPE (ma, a, SCM_ARG1, __FUNCTION__, "moment");
  SCM_ASSERT_TYPE (mb, b, SCM_ARG2, __FUNCTION__, "moment");

  return (*ma *  *mb).smobbed_copy();
}



LY_DEFINE (div_moment,"ly:div-moment", 2,0,0, (SCM a, SCM b),
	   "Divide two moments."
	   )
{
  Moment * ma = unsmob_moment (a);
  Moment * mb = unsmob_moment (b);
  SCM_ASSERT_TYPE (ma, a, SCM_ARG1, __FUNCTION__, "moment");
  SCM_ASSERT_TYPE (mb, b, SCM_ARG2, __FUNCTION__, "moment");

  return (*ma /  *mb).smobbed_copy();
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
Moment::to_string () const
{
  String s =  main_part_.to_string ();
  if (grace_part_)
    {
      s += "G" + grace_part_.to_string ();
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
std::ostream &
operator << (std::ostream &os, Moment const &m)
{
  os << m.to_string ();
  return os;
}
#endif
