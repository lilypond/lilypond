/*
  hara-kiri-line-group-grav.hh -- declare Hara_kiri_line_group_engraver

  source file of the GNU LilyPond music typesetter

  (c)  1998 Jan Nieuwenhuizen <jan@digicash.com>
*/


#ifndef HARA_KIRI_LINE_GROUP_GRAV_HH
#define HARA_KIRI_LINE_GROUP_GRAV_HH

#include "line-group-grav.hh"

/**
  Just as Line_group_engraver, but won't disgrace Lily by typesetting 
  an empty line (for orchestral scores).
  */
class Hara_kiri_line_group_engraver : public Line_group_engraver
{
public:
  TRANSLATOR_CLONE (Hara_kiri_line_group_engraver);
  DECLARE_MY_RUNTIME_TYPEINFO;

protected:
  virtual void create_line_spanner ();
};


#endif // HARA_KIRI_LINE_GROUP_GRAV_HH

