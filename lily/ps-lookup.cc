/*
  ps-lookup.cc -- implement Ps_lookup

  source file of the GNU LilyPond music typesetter

  (c)  1997--1998 Jan Nieuwenhuizen <janneke@gnu.org>
*/

#include "ps-lookup.hh"
#include "debug.hh"
#include "dimensions.hh"
#include "symtable.hh"
#include "scalar.hh"
#include "paper-def.hh"
#include "string-convert.hh"
#include "main.hh"
#include "file-results.hh"
#include "header.hh"
#include "paper-stream.hh"
#include "ps-stream.hh"
#include "ps-outputter.hh"

Ps_lookup::Ps_lookup ()
  : Lookup ()
{
}

Ps_lookup::Ps_lookup (Lookup const& s)
  : Lookup (s)
{
}

Ps_lookup::Ps_lookup (Symtables const& s)
  : Lookup (s)
{
}

Ps_lookup::~Ps_lookup ()
{
}

Atom
Ps_lookup::afm_find (String s) const
{
  return Lookup::afm_find (s, String ("(\\%03o) show "));
}

Atom*
Ps_lookup::atom_p (String s, int n, Box b) const
{
  for (int i = 0; i < n; i++)
    s.prepend ("% ");
  return new Atom (s, b);
}

String
Ps_lookup::character_str (int i) const
{
  return to_str (i, "(\\%03o)");
}

Atom
Ps_lookup::dashed_slur (Array<Offset> controls, Real thick, Real dash) const
{
  assert (controls.size () == 8);

  String ps;
  
  Real dx = controls[3].x () - controls[0].x ();
  Real dy = controls[3].y () - controls[0].y ();

  for (int i = 1; i < 4; i++)
    ps += String_convert::double_str (controls[i].x ()) + " "
      + String_convert::double_str (controls[i].y ()) + " ";

  ps += String_convert::double_str (controls[0].x ()) + " "
    + String_convert::double_str (controls[0].y ()) + " ";

  ps += String_convert::double_str (thick) + " ";
  Real on = dash > 1? thick * dash - thick : 0;
  Real off = 2 * thick;
  ps += "[" + String_convert::double_str (on) + " ";
  ps += String_convert::double_str (off) + "] ";
  ps += String_convert::int_str (0) + " ";
  ps += "draw_dashed_slur ";

  Atom a;
  a.str_ = ps;
  
  a.dim_[X_AXIS] = Interval (0, dx);
  a.dim_[Y_AXIS] = Interval (0 <? dy,  0 >? dy);
  a.font_ = font_;
  return a;
}

Atom
Ps_lookup::hairpin (Real width, bool decresc, bool continued) const
{
  Atom a;  
  Real height = paper_l_->staffheight_f () / 6;
  String ps;
  ps += to_str (width) + " " 
	+ to_str (height) + " " 
    + to_str (continued ? height/2 : 0) + 
    + " draw_"  + String (decresc ? "de" : "") + "cresc\n";
  a.str_ = ps;


  a.dim_.x () = Interval (0, width);
  a.dim_.y () = Interval (-2*height, 2*height);
  a.font_ = font_;
  return a;
}

Lookup*
Ps_lookup::lookup_p (Lookup const& l) const
{
  return new Ps_lookup (l);
}

Lookup*
Ps_lookup::lookup_p (Symtables const& s) const
{
  return new Ps_lookup (s);
}

Paper_outputter*
Ps_lookup::paper_outputter_p (Paper_stream* os_p, Paper_def* paper_l, Header* header_l, String origin_str) const
{
  if (header_global_p)
    *os_p << header_global_p->ps_string ();
  
  *os_p << _ ("\n% outputting Score, defined at: ") << origin_str << '\n';

  if (header_l)
    *os_p << header_l->ps_string ();
  *os_p << paper_l->ps_output_settings_str ();

  if (experimental_features_global_b)
    *os_p << "turnOnExperimentalFeatures\n";

  return new Ps_outputter (os_p);
}

Paper_stream*
Ps_lookup::paper_stream_p () const
{
#if 1
  String outname = base_output_str ();
#else
  String outname = "lelie";
#endif

  if (outname != "-")
    outname += ".ps";
  *mlog << _f ("PostScript output to %s...", 
	       outname == "-" ? String ("<stdout>") : outname ) << endl;
  target_str_global_array.push (outname);
  return new Ps_stream (outname);
}

Atom
Ps_lookup::plet (Real dy , Real dx, Direction dir) const
{
  String ps;
  
  ps += String_convert::double_str (dx) + " " 
    + String_convert::double_str (dy) + " "
    + String_convert::int_str ( (int)dir) +
    " draw_plet ";

  Atom s;
  s.str_ = ps;
  return s;
}

Atom
Ps_lookup::ps_beam (Real slope, Real width, Real thick) const
{
  String ps;
  ps += to_str (width) + " "+ to_str (slope) + " " + to_str (thick)
    + " draw_beam ";

  Atom s;
  s.str_ = ps;
  return s;
}

Atom
Ps_lookup::slur (Array<Offset> controls) const
{
  assert (controls.size () == 8);

  String ps;
  
  Real dx = controls[3].x () - controls[0].x ();
  Real dy = controls[3].y () - controls[0].y ();

  for (int i = 5; i < 8; i++)
    ps += String_convert::double_str (controls[i].x ()) + " "
      + String_convert::double_str (controls[i].y ()) + " ";

  ps += String_convert::double_str (controls[4].x ()) + " "
    + String_convert::double_str (controls[4].y ()) + " ";
  
  for (int i = 1; i < 4; i++)
    ps += String_convert::double_str (controls[i].x ()) + " "
      + String_convert::double_str (controls[i].y ()) + " ";

  ps += String_convert::double_str (controls[0].x ()) + " "
    + String_convert::double_str (controls[0].y ()) + " ";

  ps += " draw_slur ";

  Atom s;
  s.str_ = ps;
  
  s.dim_[X_AXIS] = Interval (0, dx);
  s.dim_[Y_AXIS] = Interval (0 <? dy,  0 >? dy);
  s.font_ = font_;
  return s;
}

Atom
Ps_lookup::stem (Real y1, Real y2) const
{
  return Lookup::stem (y1, y2, "\n% % % % draw_stem ");
}

Atom
Ps_lookup::text (String style, String text) const
{
  return Lookup::text (style, "(" + text + ")");
}

String
Ps_lookup::unknown_str () const
{
  return "unknown ";
}

Atom
Ps_lookup::vbracket (Real &y) const
{
  Atom psbracket;
  Real min_y = paper_l_->staffheight_f ();
  if (y < min_y)
    {
      warning (_ ("bracket")
	+ " " + _ ("too small") +  " (" + print_dimen (y) + ")");
//      y = min_y;
    }
  psbracket.str_ = to_str (y) + " draw_bracket ";
  psbracket.dim_[Y_AXIS] = Interval (-y/2,y/2);
  psbracket.dim_[X_AXIS] = Interval (0,4 PT);
  return psbracket;
}



