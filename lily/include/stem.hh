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
  */

struct Stem : Item {
    /// heads that the stem encompasses (positions)
    int minnote, maxnote;

    /// false if in beam
    bool print_flag;
    
    int beams_left;
    int beams_right;
    
    /// needed for determining direction/length
    int staff_center;


    /**extent of the stem (positions).
      fractional, since Beam has to adapt them.
      */

    Real bot, top;
    Real stemlen;
    
    /// flagtype? 4 none, 8 8th flag, 0 = beam.
    int flag;


    /**
      geen gedonder, jij gaat onder.
       -1 stem points down, +1: stem points up
       */

    int dir;
    Real stem_xoffset;
    
    Array<Notehead*> heads;

    /* *************** */
    Stem(int center); //, Moment duration);
    
    /// ensure that this Stem also encompasses the Notehead #n#
    void add(Notehead*n);

    NAME_MEMBERS(Stem);

    Real hindex()const;
    void do_print() const;
    void set_stemend(Real);
    int get_default_dir();
    void set_default_dir();
    void set_default_stemlen();
    void set_default_extents();
    void set_noteheads();
    void do_pre_processing();

    Interval width() const;

    Molecule* brew_molecule_p() const;
};
#endif
