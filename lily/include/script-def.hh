/*
  script-def.hh -- part of LilyPond

  (c) 1996,97 Han-Wen Nienhuys
*/

#ifndef SCRIPTDEF_HH
#define SCRIPTDEF_HH
#include "string.hh"

/** The characteristics of a certain kind of accent. It is not the
  accent itself.  */
struct Script_def {

    /// on the other side of the stem?
    int rel_stem_dir_i_;

    /// below or above staff?
    int staff_dir_i_;

    /// how close to the note do we want to be?
    int priority_i_;
    
    /// follow the ball inside staff?
    bool inside_staff_b_;

    /// invert if below staff?
    bool invertsym_b_;
    String symidx;

    /* *************** */
    int compare(Script_def const &);
    void print() const;
    Script_def(String, bool, int, int ,bool);
};


#endif // SCRIPTDEF_HH

