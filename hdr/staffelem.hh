/*
  staffelem.hh -- part of LilyPond

  (c) 1996 Han-Wen Nienhuys
*/

#ifndef STAFFELEM_HH
#define STAFFELEM_HH
#include "vray.hh"
#include "proto.hh"
#include "offset.hh"
#include "molecule.hh"

struct Staff_elem {
    enum Status {
	ORPHAN,			// not yet added to pstaff
	VIRGIN,			// added to pstaff
	PRECALCED,		// calcs before spacing done
	POSTCALCED,		// after spacing calcs done
	OUTPUT,			// molecule has been output
    } status;
    bool calc_children;
    svec<Staff_elem*> dependencies;
    
    /// indirection to the pstaff it is in
    PStaff *pstaff_;

    /****************/
    
    String TeXstring () const ;
    virtual void print() const;
    virtual Interval width() const;
    virtual Interval height() const;
    Paperdef *paper() const;
    virtual ~Staff_elem();
    Staff_elem();
    
    void translate(Offset);
    void add_processing();
    void pre_processing();
    void post_processing();
    void molecule_processing();
    
protected:
    /// generate the molecule    
    virtual Molecule* brew_molecule()const=0;
    ///executed directly after the item is added to the PScore
    virtual void do_add_processing();
    /// do calculations before determining horizontal spacing
    virtual void do_pre_processing();

    /// do calculations after determining horizontal spacing
    virtual void do_post_processing();

private:
    /// member: the symbols
    Molecule *output;		// should scrap, and use temp var?

    /// 
    Offset offset_;
    /**
      This is  needed, because #output# may still be
      NULL.
      */
};

#endif // STAFFELEM_HH

