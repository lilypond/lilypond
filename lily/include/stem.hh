/*
  stem.hh -- declare Stem

  (c) 1996--2000 Han-Wen Nienhuys
*/

#ifndef STEM_HH
#define STEM_HH

#include "item.hh"
#include "array.hh"
#include "moment.hh"
#include "molecule.hh"
#include "stem-info.hh"

/**the rule attached to the ball.
  takes care of:

  \begin{itemize}
  \item the rule
  \item the flag
  \item up/down position.
  \end{itemize}

  should move beam_{left, right} into Beam

  TODO.
  
  Stem size depends on flag.

  elt properties:

  beam_dir: direction of the beam (int)

  dir_force: is direction explicitely specified? (bool)

  /// how many abbrev beam don't reach stem?
  int beam_gap_i_;


  
  */
class Stem : public Item
{
public:
 static SCM brew_molecule (SCM);
  


  /// log of the duration. Eg. 4 -> 16th note -> 2 flags
  int flag_i () const;

  int beam_count (Direction) const;
  void set_beaming (int,  Direction d);
  /** 
    don't print flag when in beam.
    our beam, for aligning abbrev flags
   */
  Beam* beam_l () const;
  Rhythmic_head * first_head () const;
  Drul_array<Rhythmic_head*> extremal_heads () const;

  Score_element * support_head () const;
  Stem (SCM);

  /// ensure that this Stem also encompasses the Notehead #n#
  void add_head (Rhythmic_head*n);

  Stem_info calc_stem_info () const;

  Real chord_start_f () const;
  Direction get_direction () const;
  int type_i () const;
  void set_stemend (Real);
  Direction get_default_dir() const;

  int get_center_distance(Direction) const;
  int heads_i () const;

  bool invisible_b() const;
    
  /// heads that the stem encompasses (positions)
  Interval head_positions() const;

public:
  friend class Stem_tremolo;	// ugh.
  Real get_default_stem_end_position () const;
  void position_noteheads();

  Real stem_end_position () const;
  static Real off_callback (Score_element *, Axis);
  Molecule flag () const;

  SCM member_before_line_breaking ();
  static SCM before_line_breaking (SCM);
  static Interval dim_callback (Score_element *,Axis);
  SCM member_brew_molecule() const;

  void set_spacing_hints () ;
};
#endif
