/*
  hara-kiri-vertical-group-spanner.hh -- declare Har_kiri_vertical_group_spanner

  source file of the GNU LilyPond music typesetter

  (c)  1998 Jan Nieuwenhuizen <jan@digicash.com>
*/


#ifndef HARA_KIRI_VERTICAL_GROUP_SPANNER_HH
#define HARA_KIRI_VERTICAL_GROUP_SPANNER_HH

#include "vertical-group-spanner.hh"

/** 
  As Vertical_group_spanner, but keeps dependencies to notes.
 */
class Hara_kiri_vertical_group_spanner : public Vertical_group_spanner
{
public:
  DECLARE_MY_RUNTIME_TYPEINFO;

  virtual void do_post_processing ();
  virtual void add_element (Graphical_element* e);

protected:
  SCORE_ELEM_CLONE (Hara_kiri_vertical_group_spanner);

  virtual void do_break_processing ();
  virtual Molecule* brew_molecule_p () const;
};


#endif // HARA_KIRI_VERTICAL_GROUP_SPANNER_HH
