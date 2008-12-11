/*
  melody-spanner.cc -- implement Melody_spanner

  source file of the GNU LilyPond music typesetter

  (c) 2005--2008 Han-Wen Nienhuys <hanwen@xs4all.nl>

*/

#include "melody-spanner.hh"
#include "grob.hh"
#include "pointer-group-interface.hh"

/*
  TODO: this could be either item or spanner. For efficiency reasons,
  let's take item for now.
*/

		 
/*
  Interpolate stem directions for neutral stems.
 */
MAKE_SCHEME_CALLBACK (Melody_spanner, calc_neutral_stem_direction, 1);
SCM
Melody_spanner::calc_neutral_stem_direction (SCM smob)
{
  Grob *stem = unsmob_grob (smob);
  Grob *me =  unsmob_grob (stem->get_object ("melody-spanner"));
  if (!me || !me->is_live ())
    return scm_from_int (DOWN);
  
  extract_grob_set (me, "stems", stems);

  vector<Direction> dirs;
  for (vsize i = 0; i < stems.size (); i++)
    dirs.push_back (to_dir (stems[i]->get_property ("default-direction")));

  vsize last_nonneutral = VPOS;
  vsize next_nonneutral = 0;
  while (next_nonneutral != VPOS && next_nonneutral < dirs.size ()
	 &&  !dirs[next_nonneutral])
    next_nonneutral++;

  SCM retval = SCM_EOL;
  while (last_nonneutral == VPOS || last_nonneutral + 1 < dirs.size ()) 
    {
      Direction d1 = CENTER;
      Direction d2 = CENTER;
      if (last_nonneutral != VPOS)
	d1 = dirs[last_nonneutral];
      if (next_nonneutral < dirs.size ())
	d2 = dirs[next_nonneutral];

      Direction total = CENTER;
      if (d1 && d1 == d2)
	total = d1;
      else if (d1 && !d2)
	total = d1;
      else if (d2 && !d1)
	total = d2;
      else
	total = to_dir (me->get_property ("neutral-direction"));
      
      for (vsize i = last_nonneutral + 1; i <  next_nonneutral; i++)
	{
	  if (stems[i] == stem)
	    retval = scm_from_int (total);
	  else
	    stems[i]->set_property ("neutral-direction", scm_from_int (total));
	}

      last_nonneutral = next_nonneutral;
      while (last_nonneutral < dirs.size ()
	     && dirs[last_nonneutral])
	last_nonneutral++;
      next_nonneutral = last_nonneutral;
      last_nonneutral--;

      while (next_nonneutral < dirs.size ()
	     && !dirs[next_nonneutral])
	next_nonneutral++;
    }

  return retval;
}

void
Melody_spanner::add_stem (Grob *me, Grob *stem)
{
  Pointer_group_interface::add_grob (me, ly_symbol2scm ("stems"), stem);
  stem->set_object ("melody-spanner", me->self_scm ());
  stem->set_property ("neutral-direction", Melody_spanner::calc_neutral_stem_direction_proc);
}

ADD_INTERFACE (Melody_spanner,
	       "Context dependent typesetting decisions.",

	       /* properties */
	       "stems "
	       "neutral-direction "
	       );

  
