/*
  stem.hh -- declare Stem

  (c) 1996--1998 Han-Wen Nienhuys
*/

#ifndef STEM_HH
#define STEM_HH
#include "item.hh"
#include "array.hh"
#include "moment.hh"
#include "molecule.hh"


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
  */
class Stem : public Item {

    
  Drul_array<Real> yextent_drul_;
    
  /// needed for determining direction/length
  int staff_size_i_;

  /**extent of the stem (positions).
    fractional, since Beam has to adapt them.
    */


  /**
    geen gedonder, jij gaat onder.
    -1 stem points down, +1: stem points up
    */
  Direction  stem_xdir_;

  Link_array<Note_head> head_l_arr_;
  Link_array<Rest> rest_l_arr_;
    
public:

  /// how many abbrev beam don't reach stem?
  int beam_gap_i_;

  /// log of the duration. Eg. 4 -> 16th note -> 2 flags
  int flag_i_;

  /** 
    don't print flag when in beam.
    our beam, for aligning abbrev flags
   */
  Beam* beam_l_;

  int beams_left_i_;
  int beams_right_i_;

  /// maximum number of beams
  int mult_i_;

  /// direction stem (that's me)
  Direction dir_;

  /// is direction explicitely specified?
  bool dir_forced_b_;

  /// direction of the beam
  Direction beam_dir_;
    
  Stem ();
    
  /// ensure that this Stem also encompasses the Notehead #n#
  void add_head (Rhythmic_head*n);

  DECLARE_MY_RUNTIME_TYPEINFO;

  Real hpos_f () const;
  Real chord_start_f () const;
  
  int type_i () const;

  void do_print() const;
  void set_stemend (Real);
  Direction get_default_dir() const;
  Direction get_dir () const;

  int get_center_distance(Direction) const;
  void set_default_dir();
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
  virtual ~Stem ();
protected:
  virtual void do_substitute_dependency (Score_element*,Score_element*);
  virtual void do_pre_processing();
  virtual Interval do_width() const;
  Molecule* brew_molecule_p() const;
};
#endif
