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
    
    int pos_i_;
    
    Stem *stem_l_;

    /* *************** */
protected:
    Molecule *brew_molecule_p()const;
    virtual void do_substitute_dependency(Score_elem*,Score_elem*);
    virtual void do_print() const;
    virtual void do_post_processing();
    virtual void do_pre_processing();
    virtual Interval do_width() const;
private:

    void set_default_dir();
    void set_default_index();
    Symbol symbol()const;
public:
    General_script_def *specs_l_;
    
    static int compare(Script  *const&, Script *const&) ;
    Script();
    void set_stem(Stem*);
    NAME_MEMBERS();

};


#endif // SCRIPT_HH

