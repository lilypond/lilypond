/*
  paper-def.cc -- implement Paper_def

  source file of the GNU LilyPond music typesetter

  (c)  1997--1998 Han-Wen Nienhuys <hanwen@cs.uu.nl>
*/

#include <math.h>
#include "string.hh"
#include "assoc.hh"
#include "misc.hh"
#include "paper-def.hh"
#include "debug.hh"
#include "lookup.hh"
#include "assoc-iter.hh"
#include "score-engraver.hh"
#include "p-score.hh"
#include "identifier.hh"
#include "main.hh"
#include "scope.hh"
#include "dictionary-iter.hh"
#include "file-results.hh" // urg? header_global_p
#include "paper-outputter.hh"
#include "paper-stream.hh"

Paper_def::Paper_def ()
{
  lookup_p_assoc_p_ = new Assoc<int, Lookup*>;
}


Paper_def::~Paper_def ()
{
  for (Assoc_iter<int, Lookup*> ai(*lookup_p_assoc_p_); ai.ok (); ai++)
    {
      delete ai.val ();
    }
  
  delete lookup_p_assoc_p_;
}

Paper_def::Paper_def (Paper_def const&s)
  : Music_output_def (s)
{
  lookup_p_assoc_p_ = new Assoc<int, Lookup*>;
  for (Assoc_iter<int, Lookup*> ai(*s.lookup_p_assoc_p_); ai.ok (); ai++)
    {
      Lookup * l = new Lookup (*ai.val ());
      l->paper_l_ = this;
      set_lookup (ai.key(), l);
    }
}

Real
Paper_def::get_var (String s) const
{
  if (!scope_p_->elem_b (s))
    error (_f ("unknown paper variable: `%s\'", s));
  Real * p = scope_p_->elem (s)->access_Real (false);
  if (!p)
    {
      error (_ ("not a real variable"));
      return 0.0;
    }

  return *p;
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
Paper_def::set_lookup (int i, Lookup*l)
{
  if (lookup_p_assoc_p_->elem_b (i))
    {
      delete lookup_p_assoc_p_->elem (i);
    }
  l ->paper_l_ = this;
  (*lookup_p_assoc_p_)[i] = l;
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
  return get_var ("interline") /2.0 ;
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

  for (Assoc_iter<int, Lookup*> ai(*lookup_p_assoc_p_); ai.ok (); ai++)
    {
      DOUT << "Lookup: " << ai.key () ;
      ai.val ()->print ();
    }

  DOUT << "}\n";
#endif
}

Lookup const *
Paper_def::lookup_l (int i) const
{
  return (*lookup_p_assoc_p_)[i];
}

IMPLEMENT_IS_TYPE_B1 (Paper_def, Music_output_def);

int Paper_def::default_count_i_ = 0;

int
Paper_def::get_next_default_count () const
{
  return default_count_i_ ++;
}

//urg
extern char const* lily_version_number_sz ();

void
output_def (Paper_outputter* p, String key, String val)
{
  SCM args_scm =
    gh_cons (gh_str02scm (key.ch_l ()), gh_cons (gh_str02scm (val.ch_l ()), SCM_EOL));
  SCM scm =
    ly_append (ly_lambda_o (),
    ly_list1 (ly_append (ly_func_o ("lily-def"), args_scm)));
  p->output_scheme (scm);
}

void
output_header (Paper_outputter* p, Scope *head)
{
  if (!head)
    return;

  String id_str = "Lily was here";
  if (no_timestamps_global_b)
    id_str += ".";
  else
    id_str += String (", ") + lily_version_number_sz ();
  output_def (p, "lily_id_string", id_str);
  
  for (Dictionary_iter<Identifier*> i (*head); i.ok (); i++)
    {
      if (!i.val ()->access_String_identifier ())
	continue;
      
      String val = *i.val()->access_String_identifier ()->data_p_;
      output_def (p, i.key (), val);
    }
}

void
Paper_def::output_settings (Paper_outputter* p) const
{
  for (Dictionary_iter<Identifier*> i (*scope_p_); i.ok (); i++)
    output_def (p, String ("mudelapaper") + i.key (), i.val ()->str ());
  p->output_string (*scope_p_->elem (String (output_global_ch) + "setting")->access_String ());
}

Paper_outputter*
Paper_def::paper_outputter_p (Paper_stream* os_p, Header* header_l, String origin_str) const
{
  Paper_outputter* p = new Paper_outputter (os_p);

  output_header (p, header_global_p);
  p->output_comment (_ ("outputting Score, defined at: "));
  p->output_comment (origin_str);

  output_header (p, header_l);

  output_settings (p);

  SCM scm =
    ly_append (ly_lambda_o (),
    ly_list1 (ly_append (ly_func_o ("experimental-on"), SCM_EOL)));

  p->output_scheme (scm);

  scm =
    ly_append (ly_lambda_o (),
    ly_list1 (ly_append (ly_func_o ("header-end"), SCM_EOL)));

  p->output_scheme (scm);

  return p;
}

Paper_stream*
Paper_def::paper_stream_p () const
{
  String outname = base_output_str ();

  if (outname != "-")
    outname += String (".") + output_global_ch;
  *mlog << _f ("Paper output to %s...", 
	       outname == "-" ? String ("<stdout>") : outname ) << endl;
  target_str_global_array.push (outname);
  return new Paper_stream (outname);
}


String
Paper_def::base_output_str () const
{
  String str = get_default_output ();

  if (str.empty_b ())
    {
      str = default_outname_base_global;
      int def = get_next_default_count ();
      if (def)
	str += "-" + to_str (def);
    }
  return str;
}

