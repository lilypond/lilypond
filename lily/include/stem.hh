/*
  stem.hh -- declare Stem

  (c) 1996--2005 Han-Wen Nienhuys
*/

#ifndef STEM_HH
#define STEM_HH

#include "lily-proto.hh"
#include "lily-guile.hh"
#include "stem-info.hh"

class Stem 
{
public:
  static Array<int> note_head_positions (Grob *);
  static int duration_log (Grob *);
  static void set_beaming (Grob *, int, Direction d);
  static int get_beaming (Grob *, Direction d);
  static Spanner *get_beam (Grob *);
  static Grob *first_head (Grob *);
  static Grob *last_head (Grob *);
  static Drul_array<Grob*> extremal_heads (Grob *);
  static Grob *support_head (Grob *) ;
  static void add_head (Grob *me, Grob *n);
  static Stem_info get_stem_info (Grob *);
  static Real chord_start_y (Grob *);
  static Direction get_direction (Grob *);
  static void set_stemend (Grob *, Real);
  static Direction get_default_dir (Grob *);
  static Slice Stem::beam_multiplicity (Grob *);
  static Real thickness (Grob*);
  static int head_count (Grob *);
  static bool is_invisible (Grob *) ;
  static Interval head_positions (Grob *);
  static Real get_default_stem_end_position (Grob*me);
  static void position_noteheads (Grob *);
  static Real stem_end_position (Grob *);
  static Stencil flag (Grob *);
  static bool has_interface (Grob *);
  static void set_spacing_hints (Grob *);
  
  DECLARE_SCHEME_CALLBACK (print, (SCM));
  DECLARE_SCHEME_CALLBACK (offset_callback, (SCM element, SCM axis));
  DECLARE_SCHEME_CALLBACK (before_line_breaking, (SCM));
  DECLARE_SCHEME_CALLBACK (width_callback, (SCM smob, SCM axis));
  DECLARE_SCHEME_CALLBACK (height, (SCM, SCM));

private:
  static void calc_stem_info (Grob *);
};
#endif
