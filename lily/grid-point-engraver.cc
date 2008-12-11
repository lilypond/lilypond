/*
  grid-line-engraver.cc --  implement Grid_point_engraver

  source file of the GNU LilyPond music typesetter

  (c) 2005--2008 Han-Wen Nienhuys <hanwen@xs4all.nl>
*/

#include "engraver.hh"
#include "item.hh"
#include "moment.hh"

#include "translator.icc"

class Grid_point_engraver : public Engraver
{
public:
  TRANSLATOR_DECLARATIONS (Grid_point_engraver);
protected:
  void process_music ();
};

void
Grid_point_engraver::process_music ()
{
  SCM grid_interval = get_property ("gridInterval");
  if (Moment *mom = unsmob_moment (grid_interval))
    {
      Moment now = now_mom ();

      if (!now.main_part_.mod_rat (mom->main_part_))
	make_item ("GridPoint", SCM_EOL);
    }
}

Grid_point_engraver::Grid_point_engraver ()
{
}

ADD_TRANSLATOR (Grid_point_engraver,
		/* doc */
		"Generate grid points.",

		/* create */
		"GridPoint ",

		/* read */
		"gridInterval ",

		/* write */
		""
		);

