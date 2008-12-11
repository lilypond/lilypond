/*
  font-interface.hh -- declare Font_interface

  source file of the GNU LilyPond music typesetter

  (c) 2000--2008 Han-Wen Nienhuys <hanwen@xs4all.nl>
*/

#ifndef FONT_INTERFACE_HH
#define FONT_INTERFACE_HH

#include "font-metric.hh"
#include "grob-interface.hh"

struct Font_interface
{
  static SCM text_font_alist_chain (Grob *);
  static SCM music_font_alist_chain (Grob *);
  static Font_metric *get_default_font (Grob *);
  DECLARE_GROB_INTERFACE();
};

#endif /* FONT_INTERFACE_HH */
