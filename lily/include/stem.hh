/*
  stem.hh -- declare Stem

  (c) 1996,97 Han-Wen Nienhuys
*/

#ifndef STEM_HH
#define STEM_HH
#include "item.hh"
#include "varray.hh"
#include "moment.hh"


/**the rule attached to the ball.
  takes care of:

  \begin{itemize}
  \item the rule
  \item the flag
  \item up/down position.
  \end{itemize}

  should move beam_{left, right} into Beam
  */
class Stem : public Item {
    
    Real stem_bottom_f_, stem_top_f_;
    
    
    /// needed for determining direction/length
    int staff_size_i_;

    /**extent of the stem (positions).
      fractional, since Beam has to adapt them.
      */


    /**
      geen gedonder, jij gaat onder.
      -1 stem points down, +1: stem points up
      */
    Real stem_xoffset_f_;
    /**
      store the wholes (for vapourware tremolo)
     */
    Link_array<Note_head> whole_l_arr_;
    Link_array<Note_head> head_l_arr_;
    Link_array<Note_head> rest_l_arr_;
    
public:
    /// flagtype? 4 none, 8 8th flag, 0 = beam.
    int flag_i_;

    int beams_left_i_;
    int beams_right_i_;

    /// false if in beam
    bool print_flag_b_;
    
    int dir_i_;

    
    /* *************** */
    Stem(int staff_size_i);
    
    /// ensure that this Stem also encompasses the Notehead #n#
    void add(Note_head*n);

    NAME_MEMBERS(Stem);

    Real hpos_f()const;
    
    void do_print() const;
    void set_stemend(Real);
    int get_default_dir();
    int get_center_distance();
    void set_default_dir();
    void set_default_stemlen();
    void set_default_extents();
    void set_noteheads();

    Real stem_length_f()const;
    Real stem_end_f()const;
    Real stem_start_f() const;

    bool invisible_b()const;
    bool chord_b()const;
    
    /// heads that the stem encompasses (positions)
    int max_head_i() const;
    int min_head_i() const;
protected:
    virtual void do_substitute_dependency(Score_elem*,Score_elem*);
    virtual void do_pre_processing();
    virtual Interval do_width() const;
    Molecule* brew_molecule_p() const;
};
#endif
