/*
  tex-lookup.cc -- implement Tex_lookup

  source file of the GNU LilyPond music typesetter

  (c)  1997--1998 Jan Nieuwenhuizen <janneke@gnu.org>
  Jan Nieuwenhuizen <janneke@gnu.org>
*/

#include "tex-lookup.hh"
#include "debug.hh"
#include "symtable.hh"
#include "scalar.hh"
#include "paper-def.hh"
#include "string-convert.hh"
#include "main.hh"
#include "file-results.hh"
#include "scope.hh"
#include "paper-stream.hh"
#include "tex-stream.hh"
#include "tex-outputter.hh"
#include "dictionary-iter.hh"
#include "identifier.hh"

Tex_lookup::Tex_lookup ()
  : Ps_lookup ()
{
}

Tex_lookup::Tex_lookup (Lookup const& s)
  : Ps_lookup (s)
{
}

Tex_lookup::Tex_lookup (Symtables const& s)
  : Ps_lookup (s)
{
}

Tex_lookup::~Tex_lookup()
{
}

Atom
Tex_lookup::afm_find (String s) const
{
  return Lookup::afm_find (s, String ("\\char%d"));
}

Atom*
Tex_lookup::atom_p (String s, int n, Box b) const
{
  if (s.length_i ())
    s.prepend ("\\");
  for (int i = 0; i < n; i++)
    s += "{%}";
  return new Atom (s, b);
}

String
Tex_lookup::character_str (int i) const
{
  return Lookup::character_str (i);
}

Atom
Tex_lookup::dashed_slur (Array<Offset> controls, Real thick, Real dash) const
{
  return embed (Ps_lookup::dashed_slur (controls, thick, dash));
}

Atom
Tex_lookup::embed (Atom a) const
{
  a.str_ = "\\embeddedps{\n" + a.str_ + "}";
  return a;
}

Atom
Tex_lookup::hairpin (Real width, bool decresc, bool continued) const
{
  return embed (Ps_lookup::hairpin (width, decresc, continued));
}

Atom
Tex_lookup::plet (Real dy , Real dx, Direction dir) const
{
  return embed (Ps_lookup::plet (dy, dx, dir));
}

Lookup*
Tex_lookup::lookup_p (Lookup const& l) const
{
  return new Tex_lookup (l);
}

Lookup*
Tex_lookup::lookup_p (Symtables const& s) const
{
  return new Tex_lookup (s);
}

extern char const *lily_version_number_sz ();

String
header_to_tex_string (Scope *head)
{
  String s;
  String lily_id_str = "Lily was here, " +
    String (lily_version_number_sz ());
  s+= "\\def\\LilyIdString{"  + lily_id_str + "}\n";
  
  for (Dictionary_iter<Identifier*> i(*head); i.ok (); i++)
    {
      if (!i.val ()->access_String_identifier ())
	continue;
      
      String val = *i.val()->access_String_identifier ()->data_p_;
      s += "\\def\\mudela" + i.key () + "{" + val  + "}\n";
    }
  return s;
}


Paper_outputter*
Tex_lookup::paper_outputter_p (Paper_stream* os_p, Paper_def* paper_l, Scope* header_l, String origin_str) const
{
  if (header_global_p)
    *os_p << header_to_tex_string(header_global_p);
  
  *os_p << _ ("\n% outputting Score, defined at: ") << origin_str << '\n';

  if (header_l)
    *os_p << header_to_tex_string (header_global_p);
  

  *os_p << paper_l->tex_output_settings_str ();
  
  if (experimental_features_global_b)
    *os_p << "\\turnOnExperimentalFeatures%\n";

  *os_p << "\\turnOnPostScript%\n";

  return new Tex_outputter (os_p);
}

Paper_stream *
Tex_lookup::paper_stream_p () const
{
  String outname = base_output_str ();

  Paper_stream* p;
  if (outname != "-")
    outname += ".tex";
  *mlog << _f ("TeX output to %s...", 
	       outname == "-" ? String ("<stdout>") : outname ) << endl;
  p = new Tex_stream (outname);
  target_str_global_array.push (outname);
  return p;
}

String
Tex_lookup::print_dimen (Real r) const
{
  String s = to_str (r, "%.3f");
  if (s.index_i ("NaN") != -1)
    {
      warning (_ ("NaN"));
      s = "0.0";
    }
  return Lookup::print_dimen (r) + "pt";
}

Atom
Tex_lookup::ps_beam (Real slope, Real width, Real thick) const
{
  return embed (Ps_lookup::ps_beam (slope, width, thick));
}

Atom
Tex_lookup::slur (Array<Offset> controls) const
{
  return embed (Ps_lookup::slur (controls));
}

Atom
Tex_lookup::stem (Real y1, Real y2) const
{
  return Lookup::stem (y1, y2, "\\kern %\\vrule width % height % depth %");
}

Atom
Tex_lookup::text (String style, String text) const
{
  return Lookup::text (style, text);
}

String
Tex_lookup::unknown_str () const
{
  return "\\unknown";
}

Atom
Tex_lookup::vbracket (Real &y) const
{
  return embed (Ps_lookup::vbracket (y));
}

