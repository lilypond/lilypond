/*
  hara-kiri-line-group-engraver.hh -- declare Hara_kiri_line_group_engraver

  source file of the GNU LilyPond music typesetter

  (c) 1998--2001 Jan Nieuwenhuizen <janneke@gnu.org>
*/


#ifndef HARA_KIRI_LINE_GROUP_GRAV_HH
#define HARA_KIRI_LINE_GROUP_GRAV_HH

#include "line-group-group-engraver.hh"

/**
  Just as Line_group_engraver, Find interesting items for
  Hara_kiri_line_group_engraver.



  @see
  Hara_kiri_vertical_group_spanner
  

  */
class Hara_kiri_line_group_engraver : public Line_group_engraver_group
{
public:
  VIRTUAL_COPY_CONS (Translator);

protected:
  virtual void create_line_spanner ();
  void typeset_grob (Grob*);
};


#endif // HARA_KIRI_LINE_GROUP_GRAV_HH

