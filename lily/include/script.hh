/*
  script.hh -- part of GNU LilyPond

  (c) 1996,97 Han-Wen Nienhuys
*/

#ifndef SCRIPT_HH
#define SCRIPT_HH

#include "staff-side.hh"
#include "item.hh"
  
/**
  Accents that are put over a note-group.
 */
class Script : public Item, public Staff_side {
    
    Stem *stem_l_;

protected:
    Molecule *brew_molecule_p()const;
    virtual void do_substitute_dependency (Score_elem*,Score_elem*);
    virtual void do_print() const;
    virtual Interval symbol_height()const;
    virtual void do_pre_processing();
    virtual Interval do_width() const;
    SCORE_ELEM_CLONE(Script);
private:

    void set_default_dir();
public:
    General_script_def *specs_l_;
    
    static int compare (Script  *const&, Script *const&) ;
    Script();
    void set_stem (Stem*);
    DECLARE_MY_RUNTIME_TYPEINFO;

};


#endif // SCRIPT_HH

