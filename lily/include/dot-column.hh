/*
  dot-column.hh -- declare Dot_column Dot_column

  source file of the GNU LilyPond music typesetter

  (c)  1997--2002 Han-Wen Nienhuys <hanwen@cs.uu.nl>
*/


#ifndef DOT_COLUMN_HH
#define DOT_COLUMN_HH

#include "lily-guile.hh"

/**
  Group dots.  This is needed because, the dots have to be aligned per voice
 */
class Dot_column		// interface
{
public:
  static int compare (Grob * const&,Grob * const&);
  static void add_head (Grob * dotcol, Grob* rh );

  static bool has_interface (Grob*);
  DECLARE_SCHEME_CALLBACK (force_shift_callback, (SCM ,SCM));
  DECLARE_SCHEME_CALLBACK (side_position, (SCM ,SCM));  
  static SCM do_shifts (SCM dotlist);
};
#endif // DOT_COLUMN_HH
