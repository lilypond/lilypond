/*
  staff-elem.hh -- part of LilyPond

  (c) 1996,97 Han-Wen Nienhuys
*/

#ifndef STAFFELEM_HH
#define STAFFELEM_HH
#include "varray.hh"
#include "proto.hh"
#include "offset.hh"
#include "molecule.hh"
#include "class-name.hh"


/** Both Spanner and Item are Score_elem's. Most Score_elem's depend
  on other Score_elem's, eg, Beam needs to know and set direction of
  Stem. So the Beam has to be calculated *before* Stem. This is
  accomplished with the dependencies field of struct Score_elem.

  (elem)
  */
class Score_elem {

    /// member: the symbols
    Molecule *output;		// should scrap, and use temp var?


    /**
      This is  needed, because #output# may still be
      NULL.
      */
    Offset offset_;
    Array<Score_elem*> dependancy_l_arr_;
public:
    enum Status {
	ORPHAN,			// not yet added to pstaff
	VIRGIN,			// added to pstaff
	PRECALCING,
	PRECALCED,		// calcs before spacing done
	POSTCALCING,		// busy calculating. This is used to trap cyclic deps.
	POSTCALCED,		// after spacing calcs done
	VERTICALCING,		// height determined
	VERTICALCED,
	OUTPUT,			// molecule has been output
	DELETED,		// to catch malloc mistakes.
    } status;
    
    ///  the pstaff it is in
    PStaff *pstaff_l_;

    /* *************** */
    Score_elem(Score_elem const&);
    String TeXstring () const ;
    virtual void print() const;
    virtual Interval width() const;
    virtual Interval height() const;
    Paper_def *paper() const;
    virtual ~Score_elem();
    Score_elem();
    NAME_MEMBERS(Score_elem);    

    /**
      translate the symbol. The symbol does not have to be created yet. 
      Overridable, since this staff-elem might act as a pseudo-list.
     */
    virtual void translate(Offset);
    Offset offset()const;
    void add_processing();
    void pre_processing();
    void post_processing();
    void molecule_processing();
    
    virtual Spanner* spanner()  { return 0; }
    virtual Item * item() { return 0; }
    /**
      add a dependency. It may be the 0 pointer, in which case, it is ignored.
     */
    void add_dependency(Score_elem* );    
    void substitute_dependency(Score_elem* old, Score_elem * newdep);
    
protected:
    virtual  Interval do_height()const;
    virtual Interval do_width()const;
    /// do printing of derived info.
    virtual void do_print() const {}
    /// generate the molecule    
    virtual Molecule* brew_molecule_p()const;
    ///executed directly after the item is added to the PScore
    virtual void do_add_processing();
    /// do calculations before determining horizontal spacing
    virtual void do_pre_processing();

    /// do calculations after determining horizontal spacing
    virtual void do_post_processing();

    /// do calculations after height of spanners/items is determined.
    virtual void do_verticalcing();
    Array<Score_elem*> dependant_l_arr_;

};


#endif // STAFFELEM_HH

