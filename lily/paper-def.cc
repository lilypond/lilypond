/*
  paper-def.cc -- implement Paper_def

  source file of the GNU LilyPond music typesetter

  (c)  1997--1998 Han-Wen Nienhuys <hanwen@stack.nl>
*/

#include <math.h>
#include "string.hh"
#include "assoc.hh"
#include "misc.hh"
#include "paper-def.hh"
#include "debug.hh"
#include "lookup.hh"
#include "dimen.hh"
#include "assoc-iter.hh"
#include "score-grav.hh"
#include "p-score.hh"
#include "identifier.hh"
#include "main.hh"
#include "scope.hh"


Paper_def::Paper_def ()
{
  lookup_p_ = 0;
  scope_p_ = new Scope;
}

Paper_def::~Paper_def ()
{
  delete scope_p_;
  delete lookup_p_;
}

Paper_def::Paper_def (Paper_def const&s)
  : Music_output_def (s)
{
  lookup_p_ = s.lookup_p_? new Lookup (*s.lookup_p_) : 0;
  if (lookup_p_)
    {
      lookup_p_->paper_l_ = this;
    }
  scope_p_ = new Scope (*s.scope_p_);
}

Real
Paper_def::get_var (String s) const
{
  if (!scope_p_->elt_b (s))
    error (_ ("unknown paper variable `")  + s+"'");
  Real * p = scope_p_->elem (s)->real ();
  Real r = *p;
  delete p;
  return r;
}

Interval
Paper_def::line_dimensions_int (int n) const
{
  if (!shape_int_a_.size ())
    if (n)
      return Interval (0, linewidth_f ());
    else
      return Interval (get_var ("indent"), linewidth_f ());

  if (n >= shape_int_a_.size ())
    n = shape_int_a_.size () -1;

  return shape_int_a_[n];
}

Real
Paper_def::beam_thickness_f () const
{
  return get_var ("beam_thickness");
}

Real
Paper_def::linewidth_f () const
{
  return get_var ("linewidth");
}

Real
Paper_def::duration_to_dist (Moment d,Real k) const
{
  if (get_var ("geometric"))
    return geometric_spacing (d);
  return arithmetic_spacing (d,k);
}


/**
  Get the measure wide constant for arithmetic.

  @see
  John S. Gourlay. ``Spacing a Line of Music,'' Technical Report
  OSU-CISRC-10/87-TR35, Department of Computer and Information Science,
  The Ohio State University, 1987.

  */
Real
Paper_def::arithmetic_constant (Moment d) const
{
  return get_var ("arithmetic_basicspace") - log_2 (Moment (1,8) <? d);
}

Real
Paper_def::arithmetic_spacing (Moment d ,Real k) const
{
  return (log_2 (d) + k)* get_var ("arithmetic_multiplier");
}

Real
Paper_def::geometric_spacing (Moment d) const
{
  Real dur_f = (d) ?pow (get_var ("geometric"), log_2 (d)) : 0;
  return get_var ("basicspace") + get_var ("unitspace")  * dur_f;
}

void
Paper_def::set (Lookup*l)
{
  assert (l != lookup_p_);
  delete lookup_p_;
  lookup_p_ = l;
  lookup_p_->paper_l_ = this;
}

Real
Paper_def::interline_f () const
{
  return get_var ("interline");
}

Real
Paper_def::rule_thickness () const
{
  return get_var ("rulethickness");
}

Real
Paper_def::staffline_f () const
{
  return get_var ("rulethickness");
}

Real
Paper_def::staffheight_f () const
{
  return get_var ("staffheight");
}

Real
Paper_def::interbeam_f (int multiplicity_i) const
{
  if (multiplicity_i <= 3)
    return get_var ("interbeam");
  else
    return get_var ("interbeam4");
}

Real
Paper_def::internote_f () const
{
  return get_var ("internote");
}

Real
Paper_def::note_width () const
{
  return get_var ("notewidth");
}

void
Paper_def::print () const
{
#ifndef NPRINT
  Music_output_def::print ();
  DOUT << "Paper {";
  if (lookup_p_)
    lookup_p_->print ();
  for (Assoc_iter<String,Identifier*> i (*scope_p_); i.ok (); i++)
    {
      DOUT << i.key () << "= ";
//      i.val ()->print ();
// urg
      DOUT << i.val ()->str () << "\n";
    }
  DOUT << "}\n";
#endif
}

Lookup const *
Paper_def::lookup_l ()
{
  assert (lookup_p_);
  return lookup_p_;
}

IMPLEMENT_IS_TYPE_B1 (Paper_def, Music_output_def);

String
Paper_def::TeX_output_settings_str () const
{
  String s ("\n ");
  for (Assoc_iter<String,Identifier*> i (*scope_p_); i.ok (); i++)
    s += String ("\\def\\mudelapaper") + i.key () 
      + "{" + i.val ()->str () + "}\n";
  s +=  lookup_p_->texsetting + "% (Tex id)\n";
  return s;
}

int Paper_def::default_count_i_ = 0;

int
Paper_def::get_next_default_count () const
{
  return default_count_i_ ++;
}

