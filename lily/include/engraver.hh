/*
  engraver.hh -- declare Engraver

  source file of the GNU LilyPond music typesetter

  (c) 1996,  1997--1998 Han-Wen Nienhuys <hanwen@cs.uu.nl>
*/


#ifndef ENGRAVER_HH
#define ENGRAVER_HH

#include "lily-proto.hh"
#include "array.hh"
#include "request.hh"
#include "score-element-info.hh"
#include "staff-info.hh"
#include "translator.hh"


/**
  a struct which processes requests, and creates the #Score_element#s.
  It may use derived classes. Hungarian postfix: grav
  
  */
class Engraver : public virtual Translator {
    
  friend class Engraver_group_engraver;

protected:
    

  /// utility
  Paper_def * paper() const;
  /**
    Invoke walker method to typeset element. Default: pass on to daddy.
    */
  virtual void typeset_element (Score_element*elem_p);
    
  /**
    take note of item/spanner
    put item in spanner. Adjust local key; etc.

    Default: ignore the info
    */
  virtual void acknowledge_element (Score_element_info) {}

  /** Do things with stuff found in acknowledge_element. Ugh. Should
     be looped with acknowledge_element.
     
   */
  virtual void process_acknowledged () {}
  /**
    Announce element. Default: pass on to daddy. Utility
    */
  virtual void announce_element (Score_element_info);
   
  /**
    Get information on the staff. Default: ask daddy.
    */
  virtual Staff_info get_staff_info() const;
  virtual void fill_staff_info (Staff_info&);

public:
  TRANSLATOR_CLONE(Engraver);
  Engraver_group_engraver * daddy_grav_l() const;
  /**
    override other ctor
   */
  Engraver () {}

  DECLARE_MY_RUNTIME_TYPEINFO;
  virtual Engraver * access_Engraver () { return this; }
};


#endif // ENGRAVER_HH

