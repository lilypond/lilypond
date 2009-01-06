/*
  axis-group-engraver.hh -- declare Axis_group_engraver

  source file of the GNU LilyPond music typesetter

  (c) 2005--2009 Han-Wen Nienhuys <hanwen@xs4all.nl>
*/

#ifndef AXIS_GROUP_ENGRAVER_HH
#define AXIS_GROUP_ENGRAVER_HH

#include "engraver.hh"

/**
   Put stuff in a Spanner with an Axis_group_interface.
   Use as last element of a context.
*/
class Axis_group_engraver : public Engraver
{
protected:
  Spanner *staffline_;
  vector<Grob*> elts_;
  void process_music ();
  virtual void finalize ();
  DECLARE_ACKNOWLEDGER (grob);
  void process_acknowledged ();
  virtual Spanner *get_spanner ();
  virtual void add_element (Grob *);
public:
  TRANSLATOR_DECLARATIONS (Axis_group_engraver);
};
#endif /* AXIS_GROUP_ENGRAVER_HH */
