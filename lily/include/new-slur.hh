/* 
  new-slur.hh -- declare New_slur
  
  source file of the GNU LilyPond music typesetter
  
  (c) 2004 Han-Wen Nienhuys <hanwen@xs4all.nl>
  
*/

#ifndef NEW_SLUR_HH
#define NEW_SLUR_HH

#include "lily-proto.hh"
#include "lily-guile.hh"

#define DEBUG_SLUR_QUANTING 1

class New_slur
{
public:
  static void add_column (Grob *me, Grob *col);
  static void add_extra_encompass (Grob *me, Grob *col);
  DECLARE_SCHEME_CALLBACK (print, (SCM));
  DECLARE_SCHEME_CALLBACK (after_line_breaking, (SCM));
  DECLARE_SCHEME_CALLBACK (height, (SCM,SCM));

  static void set_interface (Grob *);
  static bool  has_interface (Grob *);
  static Bezier get_curve (Grob*me);
};



#endif /* NEW_SLUR_HH */

