#if 0
/*   
  note-spacing-engraver.cc --  implement  Note_spacing_engraver.

  source file of the GNU LilyPond music typesetter

  (c) 2001 Han-Wen Nienhuys <hanwen@cs.uu.nl>

*/

#include "grob.hh"
#include "moment.hh"
#include "engraver.hh"
#include "note-spacing.hh"
#include "note-column.hh"

/*
  Originally, we tried to have this functionality at Staff_level
  
  - by simply using the sequence of Separation-item as
  spacing-sequences. Unfortunately, this fucks up if there are
  different kinds of tuplets combined (8th and 8ths triplets combined
  made the program believe there were 1/12 th notes.).


  - We also created them from Rhythmic_column_engraver, but this has
  the problem that voices can appear and disappear at will, leaving
  lots of loose ends (the StaffSpacing don't know where to connect the
  last note of the voice on the right with)
  
 */

struct Grob_moment_tuple
{
  Link_array<Grob> current_heads_;
  Link_array<Grob> todo_heads_;
  
  Moment length_;
  
  static int time_compare (Grob_moment_tuple const &a, Grob_moment_tuple const &b)
  {
    return Moment::compare (a.length_, b.length_);
  }
};

class Note_spacing_engraver : public Engraver
{
public:
  TRANSLATOR_DECLARATIONS(Note_spacing_engraver);


protected:
  Array<Grob_moment_tuple> lengths_found_;

  virtual void acknowledge_grob (Grob_info);
};

Note_spacing_engraver::Note_spacing_engraver()
{
}


void
Note_spacing_engraver::acknowledge_grob (Grob_info gi)
{
  if (Note_head::has_interface (gi.grob_l_))
    {
      Music *m = gi.music_cause();
      Moment now = now_mom ();
      Moment len = m->length_mom(); 
      if (now.grace_part_ && len.main_part_)
	{
	  len.grace_part_ += len.main_part_;
	  len.main_part_ = 0;
	}
      
      for (int  i=0; i <  
    }
  Note_column::has_interface (gi.grob_l_))
    {
      Grob *head  =Note_column::first_head (gi.grob_l_);

      head->
    }
}



ENTER_DESCRIPTION(Note_spacing_engraver,
/* descr */       "This engraver creates spacing objects. It should be placed at staff
level, but will also function at voice level.

",
/* creats*/       "NoteSpacing",
/* acks  */       "rhythmic-column-interface",
/* reads */       "",
/* write */       "");

#endif
