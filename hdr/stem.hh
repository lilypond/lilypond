/*
  stem.hh -- 

  (c) 1996 Han-Wen Nienhuys
*/

#ifndef STEM_HH
#define STEM_HH
#include "item.hh"

/// the rule attached to the ball
struct Stem : public Item {
    // heads the stem encompasses (positions)
    int minnote, maxnote;

    /// false if in beam
    bool print_flag;
    
    /// needed for determining direction/length
    int staff_center;

    // extent of the stem (positions)
    Real bot, top;
    /**
      fractional, since Beam has to adapt them.
      */

    Real stemlen;
    // flagtype? 4 none, 8 8th flag, 0 = beam.
    int flag;

    ///geen gedonder, jij gaat onder
    int dir;
    /**
       -1 stem points down, +1: stem points up
       */

    Real stem_xoffset;
    /****************/

    /// ensure that this Stem also encompasses the Notehead #n#
    void add(Notehead*n);
    Real hpos()const;
    void set_stemend(Real);
    void set_default_dir();
    void set_default_stemlen();
    void set_default_extents();
    void postprocess();
    void preprocess();
    Stem(int center);
    void print() const;
    Interval width() const;
private:

    void brew_molecole();
};
/**
  takes care of:

  \begin{itemize}
  \item the rule
  \item the flag
  \item up/down position.
  \end{itemize}
  */

#endif
