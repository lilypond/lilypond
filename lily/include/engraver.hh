/*
  engraver.hh -- declare Engraver

  source file of the GNU LilyPond music typesetter

  (c) 1996--2005 Han-Wen Nienhuys <hanwen@cs.uu.nl>
*/


#ifndef ENGRAVER_HH
#define ENGRAVER_HH

#include "event.hh"
#include "grob-info.hh"
#include "translator.hh"

/**
  a struct which processes events, and creates the #Grob#s.
  It may use derived classes. 
  */
class Engraver : public virtual Translator {
    
  friend class Engraver_group_engraver;
protected:
  /*
    take note of item/spanner
    put item in spanner. Adjust local key; etc.

    Default: ignore the info
    */
  virtual void acknowledge_grob (Grob_info) {}

  /** Do things with stuff found in acknowledge_grob. Ugh. Should
     be looped with acknowledge_grob.
     
   */
  virtual void process_acknowledged_grobs () {}

  virtual void announce_grob (Grob_info);
  Engraver_group_engraver*get_daddy_engraver () const;
  
public:
  /**
    Announce element. Default: pass on to daddy. Utility
    */
  void announce_grob (Grob*, SCM cause);
  
  Score_engraver * get_score_engraver () const;
  /**
    override other ctor
   */
  TRANSLATOR_DECLARATIONS(Engraver);
};

#define make_item(x,cause) make_item_from_properties (this, ly_symbol2scm (x), cause, x)
#define make_spanner(x,cause) make_spanner_from_properties (this, ly_symbol2scm (x), cause, x)
Item* make_item_from_properties (Engraver * tg, SCM x, SCM cause, const char *name);
Spanner* make_spanner_from_properties (Engraver * tg, SCM x, SCM cause, const char*name);




#endif // ENGRAVER_HH
