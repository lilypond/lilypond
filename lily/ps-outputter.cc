/*
  ps-outputter.cc -- implement Ps_outputter

  source file of the GNU LilyPond music typesetter

  (c)  1997--1998 Han-Wen Nienhuys <hanwen@cs.uu.nl>
  Jan Nieuwenhuizen <janneke@gnu.org>
*/

#include "ps-outputter.hh"
#include "ps-stream.hh"
#include "molecule.hh"
#include "atom.hh"
#include "array.hh"
#include "string-convert.hh"
#include "debug.hh"

Ps_outputter::Ps_outputter (Paper_stream *s)
  :Paper_outputter (s)
{
}

Ps_outputter::~Ps_outputter ()
{
}

/*
   26 fonts ought to be enough for anyone.
*/
static String
ps_font_command(int i)
{
// urg
//  return "%\\font" + String_convert::form_str ("%c",  'A' + i) + "\n";
  return "\n/feta20 findfont 12 scalefont setfont ";
}

void
Ps_outputter::switch_to_font (String fontname)
{
  if (!fontname.length_i () || fontname == current_font_)
    return;

  current_font_ = fontname;
  int i=0;
  for (; i< font_arr_.size (); i++)
    if (font_arr_[i] == fontname)
      {
	*outstream_l_ <<ps_font_command (i) << "\n";
	return ;
      }

  
  font_arr_.push (fontname);
  *outstream_l_ << "%\\font"  + ps_font_command (i) << "% =" + fontname << "\n";
  *outstream_l_<< ps_font_command (i) << "\n";
}

void
Ps_outputter::output_molecule (Molecule const*m, Offset o, char const *nm)
{
  if (check_debug)
    *outstream_l_ << String ("\n%start: ") << nm << "\n";

  Paper_outputter::output_molecule (m, o, nm, "% % {%}placebox \n");
}

void
Ps_outputter::start_line ()
{
  *outstream_l_ << "\nstart_line {\n";
}

void
Ps_outputter::stop_line ()
{
  *outstream_l_ << "}\nstop_line\n";
  current_font_ = "";
  font_arr_.clear ();
}
