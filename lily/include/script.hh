/*
  script.hh -- part of LilyPond

  (c) 1996,97 Han-Wen Nienhuys
*/

#ifndef SCRIPT_HH
#define SCRIPT_HH

#include "staff-side.hh"
#include "script-def.hh"
#include "item.hh"
  
/**
  Accents that are put over a note-group.
 */
class Script : public Item, public Staff_side {
     /**
      Vertical dir of symbol. -1 means invert the symbol.
     */
    int symdir_i_;
    
    int pos_i_;
    
    Script_def *specs_l_;
    Stem *stem_l_;

    /* *************** */
protected:
    Molecule *brew_molecule_p()const;
    virtual void do_post_processing();
    virtual void do_pre_processing();
    virtual Interval do_width() const;
private:
    void set_symdir();
    void set_default_dir();
    void set_default_index();
    Symbol symbol()const;
public:
    static int compare(Script  *const&, Script *const&) ;
    Script(Script_req*);
    void set_stem(Stem*);
    NAME_MEMBERS(Script);

};


#endif // SCRIPT_HH

