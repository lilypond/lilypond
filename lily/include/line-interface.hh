/*   
line-interface.hh -- declare 

source file of the GNU LilyPond music typesetter

(c) 2004 Han-Wen Nienhuys <hanwen@xs4all.nl>

 */

#ifndef LINE_INTERFACE_HH
#define LINE_INTERFACE_HH

#include "lily-proto.hh"

struct Line_interface
{
  static Molecule dashed_line (Grob *me, Real thick, Offset from, Offset to);
  static bool has_interface (Grob*);
  static Molecule make_dashed_line (Real th, Offset from, Offset to, Real, Real);

};
#endif /* LINE_INTERFACE_HH */


