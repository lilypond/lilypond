/*
  engraver.hh -- declare Engraver

  source file of the GNU LilyPond music typesetter

  (c) 1996--2001 Han-Wen Nienhuys <hanwen@cs.uu.nl>
*/


#ifndef ENGRAVER_HH
#define ENGRAVER_HH

#include "lily-proto.hh"
#include "array.hh"
#include "request.hh"
#include "grob-info.hh"
#include "translator.hh"


/**
  a struct which processes requests, and creates the #Grob#s.
  It may use derived classes. 
  */
class Engraver : public virtual Translator {
    
  friend class Engraver_group_engraver;
protected:
  /*
    Call this when you're finished with ELEM_P.
   */
  virtual void typeset_grob (Grob*elem_p);
  /*
    take note of item/spanner
    put item in spanner. Adjust local key; etc.

    Default: ignore the info
    */
  virtual void acknowledge_grob (Grob_info) {}

  /** Do things with stuff found in acknowledge_grob. Ugh. Should
     be looped with acknowledge_grob.
     
   */
  virtual void create_grobs () {}
  /**
    Announce element. Default: pass on to daddy. Utility
    */
  virtual void announce_grob (Grob*, Music*);
  virtual void announce_grob (Grob_info);
  virtual void process_music ();
public:
  VIRTUAL_COPY_CONS(Translator);
  Engraver_group_engraver * daddy_grav_l() const;
  /**
    override other ctor
   */
  Engraver () {}
};


#endif // ENGRAVER_HH

