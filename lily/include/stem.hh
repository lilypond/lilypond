/*
  stem.hh -- declare Stem

  (c) 1996--2000 Han-Wen Nienhuys
*/

#ifndef STEM_HH
#define STEM_HH

#include "lily-proto.hh"
#include "lily-guile.hh"
#include "stem-info.hh"

class Stem 
{
public:
  DECLARE_SCHEME_CALLBACK(brew_molecule, (SCM ));

  static  int flag_i (Score_element*) ;
  static int beam_count (Score_element*,Direction) ;
  static void set_beaming (Score_element*,int,  Direction d);
  static Score_element * beam_l (Score_element*);
  static Score_element * first_head (Score_element*) ;
  static Drul_array<Score_element*> extremal_heads (Score_element*);
  static Score_element * support_head (Score_element*) ;
  static void add_head (Score_element*me, Score_element*n);
  static Stem_info calc_stem_info (Score_element *) ;
  static Real chord_start_f (Score_element *) ;
  static Direction get_direction (Score_element*) ;
  static int type_i (Score_element *) ;
  static void set_stemend (Score_element *,Real);
  static Direction get_default_dir(Score_element *) ;
  static int get_center_distance(Score_element *,Direction) ;
  static int heads_i (Score_element *) ;
  static bool invisible_b(Score_element *) ;
  static Interval head_positions(Score_element *) ;
  static Real get_default_stem_end_position (Score_element*me) ;
  static void position_noteheads(Score_element*);
  static Real stem_end_position (Score_element*) ;
  DECLARE_SCHEME_CALLBACK(off_callback, (SCM element, SCM axis));
  static Molecule flag (Score_element*);
  DECLARE_SCHEME_CALLBACK(before_line_breaking, (SCM ));
  DECLARE_SCHEME_CALLBACK(dim_callback, (SCM smob, SCM axis));
  static bool has_interface (Score_element*);
  static void set_interface (Score_element*);

  static void set_spacing_hints (Score_element*me) ;
};
#endif
