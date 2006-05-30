/*
  engraver.hh -- declare Engraver

  source file of the GNU LilyPond music typesetter

  (c) 1996--2006 Han-Wen Nienhuys <hanwen@xs4all.nl>
*/

#ifndef ENGRAVER_HH
#define ENGRAVER_HH

#include "music.hh"
#include "grob-info.hh"
#include "translator.hh"

/**
   a struct which processes events, and creates the #Grob#s.
   It may use derived classes.
*/
class Engraver : public Translator
{

  friend class Engraver_group;
protected:
  /*
    take note of item/spanner
    put item in spanner. Adjust local key; etc.

    Default: ignore the info
  */
  virtual void acknowledge_grob (Grob_info) {}
  virtual void announce_grob (Grob_info);
  virtual void announce_end_grob (Grob_info);
  Engraver_group *get_daddy_engraver () const;

public:
  /**
     Announce element. Default: pass on to daddy. Utility
  */
  void announce_grob (Grob *, SCM cause);
  void announce_end_grob (Grob *, SCM cause);

  /**
     override other ctor
  */
  TRANSLATOR_DECLARATIONS (Engraver);
};

#define make_item(x, cause) make_item_from_properties (this, ly_symbol2scm (x), cause, x)
#define make_spanner(x, cause) make_spanner_from_properties (this, ly_symbol2scm (x), cause, x)
#define make_paper_column(x) make_paper_column_from_properties (this, ly_symbol2scm (x), x)
Grob *make_grob_from_properties (Engraver *tr, SCM symbol, SCM cause, char const *name);
Item *make_item_from_properties (Engraver *tg, SCM x, SCM cause, char const *name);
Spanner *make_spanner_from_properties (Engraver *tg, SCM x, SCM cause, char const *name);
Paper_column *make_paper_column_from_properties (Engraver *tg, SCM x, char const *name);

#endif // ENGRAVER_HH
