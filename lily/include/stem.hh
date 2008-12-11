/*
  stem.hh -- declare Stem

  (c) 1996--2008 Han-Wen Nienhuys
*/

#ifndef STEM_HH
#define STEM_HH

#include "std-vector.hh"
#include "lily-proto.hh"
#include "stem-info.hh"
#include "grob-interface.hh"

class Stem
{
public:
  static vector<int> note_head_positions (Grob *);
  static int duration_log (Grob *);
  static void set_beaming (Grob *, int, Direction d);
  static int get_beaming (Grob *, Direction d);
  static Spanner *get_beam (Grob *);
  static Grob *first_head (Grob *);
  static Grob *last_head (Grob *);
  static Drul_array<Grob *> extremal_heads (Grob *);
  static Grob *support_head (Grob *);
  static void add_head (Grob *me, Grob *n);
  static Stem_info get_stem_info (Grob *);
  static Real chord_start_y (Grob *);
  static void set_stemend (Grob *, Real);
  static Slice beam_multiplicity (Grob *);
  static Direction get_default_dir (Grob*);
  static Real thickness (Grob *);
  static int head_count (Grob *);
  static bool is_invisible (Grob *);
  static bool is_normal_stem (Grob *);
  static bool is_cross_staff (Grob *);
  static Interval head_positions (Grob *);
  static Real stem_end_position (Grob *);
  static Stencil flag (Grob *);
  static Stencil get_translated_flag (Grob*);
  DECLARE_GROB_INTERFACE();
  static void set_spacing_hints (Grob *);
  
  DECLARE_SCHEME_CALLBACK (print, (SCM));
  DECLARE_SCHEME_CALLBACK (calc_default_direction, (SCM));
  DECLARE_SCHEME_CALLBACK (offset_callback, (SCM element));
  DECLARE_SCHEME_CALLBACK (calc_direction, (SCM));
  DECLARE_SCHEME_CALLBACK (calc_beaming, (SCM));
  DECLARE_SCHEME_CALLBACK (calc_length, (SCM));
  DECLARE_SCHEME_CALLBACK (calc_stem_end_position, (SCM));
  DECLARE_SCHEME_CALLBACK (calc_stem_info, (SCM));
  DECLARE_SCHEME_CALLBACK (calc_positioning_done, (SCM));
  DECLARE_SCHEME_CALLBACK (width, (SCM smob));
  DECLARE_SCHEME_CALLBACK (pure_height, (SCM, SCM, SCM));
  DECLARE_SCHEME_CALLBACK (height, (SCM));
  DECLARE_SCHEME_CALLBACK (calc_cross_staff, (SCM));
  DECLARE_SCHEME_CALLBACK (calc_flag, (SCM));
};
#endif
