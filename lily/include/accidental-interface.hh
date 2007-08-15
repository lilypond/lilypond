/*
  accidental-interface.hh -- declare  Accidental_interface

  source file of the GNU LilyPond music typesetter

  (c) 2002--2007 Han-Wen Nienhuys <hanwen@xs4all.nl>
*/

#ifndef ACCIDENTAL_INTERFACE_HH
#define ACCIDENTAL_INTERFACE_HH

#include "std-vector.hh"

#include "box.hh"
#include "lily-guile.hh"
#include "lily-proto.hh"

class Accidental_interface
{
public:
  DECLARE_SCHEME_CALLBACK (print, (SCM));
  DECLARE_SCHEME_CALLBACK (after_line_breaking, (SCM));
  
  static bool has_interface (Grob *);
  static string get_fontcharname (string style, int alteration);
  static vector<Box> accurate_boxes (Grob *me,
							  Grob **common);
};

#endif
