/*
  clef-item.hh -- declare Clef_item

  (c) 1996,97 Han-Wen Nienhuys
*/

#ifndef CLEFITEM_HH
#define CLEFITEM_HH
#include "item.hh"

/**
  Set a clef in a staff.
 */
class Clef_item : public Item {
protected:
    virtual void do_pre_processing();
    Molecule* brew_molecule_p()const;
public:
    
    String type_;
    int y_off;

    /// is this a change clef (smaller size)?
    bool change_b_;
    
    /// set because of existence of a bar
    bool default_b_;

    /* *************** */
    DECLARE_MY_RUNTIME_TYPEINFO;
    SCORE_ELEM_CLONE(Clef_item);
    Clef_item();
    void read(Clef_engraver const&);
    void read(String);
};

#endif // CLEFITEM_HH


