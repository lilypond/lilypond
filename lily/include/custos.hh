/*
  custos.hh

  source file of the GNU LilyPond music typesetter

  (c) 2000--2007 Juergen Reuter <reuter@ipd.uka.de>
*/

#ifndef CUSTOS_HH
#define CUSTOS_HH

#include "lily-guile.hh"

class Grob;
class Stencil;

struct Custos
{
  DECLARE_SCHEME_CALLBACK (print, (SCM));
  static bool has_interface (Grob *);

private:
  static void add_streepjes (Grob *me, int pos, int interspaces, Stencil *custos_);
  static Stencil create_ledger_line (Interval x_extent, Grob *me);
};

#endif // CUSTOS_HH

