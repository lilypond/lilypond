/*
  hara-kiri-vertical-group-spanner.hh -- declare Har_kiri_vertical_group_spanner

  source file of the GNU LilyPond music typesetter

  (c)  1998 Jan Nieuwenhuizen <janneke@gnu.org>
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
  

  Hara_kiri_vertical_group_spanner ();
  virtual void do_post_processing ();
  void add_note (Note_head* n);

protected:
  VIRTUAL_COPY_CONS(Score_element);

  virtual void do_substitute_dependency (Score_element*, Score_element*);
  virtual void do_print ()const;

  Link_array<Note_head> head_l_arr_;
};


#endif // HARA_KIRI_VERTICAL_GROUP_SPANNER_HH
