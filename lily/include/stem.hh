/*
  stem.hh -- declare Stem

  (c) 1996--2002 Han-Wen Nienhuys
*/

#ifndef STEM_HH
#define STEM_HH

#include "lily-proto.hh"
#include "lily-guile.hh"
#include "stem-info.hh"
#include "drul-array.hh"

class Stem 
{
public:
  DECLARE_SCHEME_CALLBACK (brew_molecule, (SCM ));

  static Array<int> note_head_positions (Grob*);
  static  int duration_log (Grob*) ;
  static void set_beaming (Grob*,int,  Direction d);
  static Grob * get_beam (Grob*);
  static Grob * first_head (Grob*) ;
  static Grob * last_head  (Grob*) ;
  static Drul_array<Grob*> extremal_heads (Grob*);
  static Grob * support_head (Grob*) ;
  static void add_head (Grob*me, Grob*n);
  static Stem_info calc_stem_info (Grob *) ;
  static Real chord_start_y (Grob *) ;
  static Direction get_direction (Grob*) ;
  static void set_stemend (Grob *,Real);
  static Direction get_default_dir (Grob *) ;
  static Slice Stem::beam_multiplicity (Grob *stem);

  static int head_count (Grob *) ;
  static bool invisible_b (Grob *) ;
  static Interval head_positions (Grob *) ;
  static Real get_default_stem_end_position (Grob*me) ;
  static void position_noteheads (Grob*);
  static Real stem_end_position (Grob*) ;
  DECLARE_SCHEME_CALLBACK (off_callback, (SCM element, SCM axis));
  static Molecule flag (Grob*);
  DECLARE_SCHEME_CALLBACK (before_line_breaking, (SCM ));
  DECLARE_SCHEME_CALLBACK (dim_callback, (SCM smob, SCM axis));
  DECLARE_SCHEME_CALLBACK (height, (SCM,SCM));
  static bool has_interface (Grob*);
  static void set_spacing_hints (Grob*me) ;
};
#endif
