/*
  stem.hh -- declare Stem

  (c) 1996--1999 Han-Wen Nienhuys
*/

#ifndef STEM_HH
#define STEM_HH
#include "item.hh"
#include "array.hh"
#include "moment.hh"
#include "molecule.hh"
#include "staff-symbol-referencer.hh"

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
// todo: remove baseclass Staff_symbol_referencer, since stem
// can be across a staff.
class Stem : public Item, public Staff_symbol_referencer {

  /**extent of the stem (positions).
    fractional, since Beam has to adapt them.
    */
  Drul_array<Real> yextent_drul_;

public:
  Link_array<Note_head> head_l_arr_;
  Link_array<Rest> rest_l_arr_;
    
  /// log of the duration. Eg. 4 -> 16th note -> 2 flags
  int flag_i_;

  /** 
    don't print flag when in beam.
    our beam, for aligning abbrev flags
   */
  Beam* beam_l_;

  Drul_array<int> beams_i_drul_;

  void set_direction (Direction d);
  /// direction stem (that's me)
  Direction dir_;


  Stem ();
    
  /// ensure that this Stem also encompasses the Notehead #n#
  void add_head (Rhythmic_head*n);

  Real hpos_f () const;
  Real chord_start_f () const;
  
  int type_i () const;

  void do_print() const;
  void set_stemend (Real);
  Direction get_default_dir() const;
  Direction get_dir () const;

  int get_center_distance(Direction) const;

  void set_default_stemlen();
  void set_default_extents();
  void set_noteheads();

  Real stem_length_f() const;
  Real stem_end_f() const;
  Real stem_begin_f() const;
  Real note_delta_f () const;

  bool invisible_b() const;
    
  /// heads that the stem encompasses (positions)
  Interval_t<int> head_positions() const;

protected:
  Molecule flag () const;
  virtual void do_substitute_element_pointer (Score_element*,Score_element*);
  virtual void do_pre_processing();
  virtual Interval do_width() const;
  virtual Molecule* do_brew_molecule_p() const;

  void set_spacing_hints () ;
};
#endif
