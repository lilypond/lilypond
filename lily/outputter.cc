
/*
  outputter.cc -- implement Tex_outputter

  source file of the GNU LilyPond music typesetter

  (c)  1997--1998 Han-Wen Nienhuys <hanwen@stack.nl>
*/

#include "outputter.hh"
#include "tex-stream.hh"
#include "molecule.hh"
#include "varray.hh"
#include "dimen.hh"
#include "tex.hh"

Tex_outputter::Tex_outputter (Tex_stream *s)
{
  outstream_l_ = s;
}

void
Tex_outputter::output_molecule (Molecule const*m, Offset o)
{
  String t = m->TeX_string();
  if (t.empty_b ())
    return ;

  String s ("\\placebox{%}{%}{%}");
  Array<String> a;
  a.push (print_dimen (o.y()));
  a.push (print_dimen (o.x()));
  a.push (t);
  String r;
  /*
  if (check_debug)
    r = String ("\n%start: ") + name() + "\n";
    */
  
  r += substitute_args (s, a);
  
  *outstream_l_ << r;
}


void
Tex_outputter::start_line ()
{
  *outstream_l_ << "\\hbox{%<- line of score\n";
}

void
Tex_outputter::stop_line ()
{
  *outstream_l_ << "}";
  *outstream_l_ << "\\interscoreline";
}
