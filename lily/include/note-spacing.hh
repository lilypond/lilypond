/*   
note-spacing.hh -- declare Note_spacing

source file of the GNU LilyPond music typesetter

(c) 2001 Han-Wen Nienhuys <hanwen@cs.uu.nl>

 */

#ifndef NOTE_SPACING_HH
#define NOTE_SPACING_HH

#include "lily-proto.hh"

class Note_spacing
{
public:
  static bool has_interface (Grob*);

  static Real get_spacing (Grob *me);
  static Real stem_dir_correction (Grob *me);  
  static Item * right_column (Grob*);
  static Item * left_column (Grob*);  
};

#endif /* NOTE_SPACING_HH */
