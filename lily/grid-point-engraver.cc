/*
  grid-line-engraver.cc --  implement Grid_point_engraver

  source file of the GNU LilyPond music typesetter

  (c) 2005 Han-Wen Nienhuys <hanwen@xs4all.nl>

*/

#include "engraver.hh"
#include "item.hh"

class Grid_point_engraver : public Engraver
{
public:
  TRANSLATOR_DECLARATIONS (Grid_point_engraver);
protected:
  virtual void process_music ();
};

void
Grid_point_engraver::process_music ()
{
  SCM  grid_interval = get_property ("gridInterval");
  if (Moment *mom = unsmob_moment (grid_interval))
    {
      Moment now = now_mom ();

      if (!now.main_part_.mod_rat (mom->main_part_))
	{
	  Item * it = make_item ("GridPoint", SCM_EOL);
	}
    }
}

Grid_point_engraver::Grid_point_engraver ()
{
}

ADD_TRANSLATOR (Grid_point_engraver,
		/* descr */ "generate grid points.",
		/* creats*/ "GridPoint",
		/* accepts */ "",
		/* acks  */ "",
		/* reads */ "gridInterval",
		/* write */ "");

