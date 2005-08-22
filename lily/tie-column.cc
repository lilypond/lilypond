/*
  tie-column.cc -- implement Tie_column

  source file of the GNU LilyPond music typesetter

  (c) 2000--2005 Han-Wen Nienhuys <hanwen@cs.uu.nl>
*/


#include <math.h>
#include <map>

#include "staff-symbol-referencer.hh"
#include "warn.hh"
#include "tie-column.hh"
#include "paper-column.hh"
#include "spanner.hh"
#include "pointer-group-interface.hh"
#include "tie.hh"
#include "directional-element-interface.hh"
#include "rhythmic-head.hh"

void
Tie_column::add_tie (Grob *me, Grob *tie)
{
  if (tie->get_parent (Y_AXIS)
      && Tie_column::has_interface (tie->get_parent (Y_AXIS)))
    return;

  if (!Pointer_group_interface::count (me, ly_symbol2scm ("ties")))
    {
      dynamic_cast<Spanner *> (me)->set_bound (LEFT, Tie::head (tie, LEFT));
      dynamic_cast<Spanner *> (me)->set_bound (RIGHT, Tie::head (tie, RIGHT));
    }

  tie->set_parent (me, Y_AXIS);
  Pointer_group_interface::add_grob (me, ly_symbol2scm ("ties"), tie);
  tie->add_dependency (me);
}

void
Tie_column::set_directions (Grob *me)
{
  if (!to_boolean (me->get_property ("positioning-done")))
    {
      me->set_property ("positioning-done", SCM_BOOL_T); 
      new_directions (me);
    }
}

int
Tie::compare (Grob *const &s1,
	      Grob *const &s2)
{
  return sign (Tie::get_position (s1) - Tie::get_position (s2));
}

/*
  Werner:

  . The algorithm to choose the direction of the ties doesn't work
  properly.  I suggest the following for applying ties sequentially
  from top to bottom:

  + The topmost tie is always `up'.

  + If there is a vertical gap to the last note above larger than
  or equal to a fifth (or sixth?), the tie is `up', otherwise it
  is `down'.

  + The bottommost tie is always `down'.
*/
void
Tie_column::werner_directions (Grob *me)
{
  extract_grob_set (me, "ties", ro_ties);
  Link_array<Grob> ties (ro_ties);
  if (!ties.size ())
    return;

  ties.sort (&Tie::compare);

  Direction d = get_grob_direction (me);
  if (d)
    {
      for (int i = ties.size (); i--;)
	{
	  Grob *t = ties[i];
	  if (!get_grob_direction (t))
	    set_grob_direction (t, d);
	}
      return;
    }

  if (ties.size () == 1)
    {
      Grob *t = ties[0];
      if (t->is_live ()
	  && !get_grob_direction (t))
	set_grob_direction (t, Tie::get_default_dir (t));
      return;
    }

  Real last_down_pos = 10000;
  if (!get_grob_direction (ties[0]))
    set_grob_direction (ties[0], DOWN);

  /*
    Go downward.
  */
  Grob *last_tie = 0;
  for (int i = ties.size (); i--;)
    {
      Grob *t = ties[i];

      Direction d = get_grob_direction (t);
      Real p = Tie::get_position (t);
      if (!d)
	{
	  if (last_tie
	      && Tie::get_column_rank (t, LEFT)
	      < Tie::get_column_rank (last_tie, LEFT))
	    d = DOWN;
	  else if (last_down_pos - p > 5)
	    d = UP;
	  else
	    d = DOWN;

	  set_grob_direction (t, d);
	}

      if (d == DOWN)
	last_down_pos = p;

      last_tie = t;
    }

  return;
}

MAKE_SCHEME_CALLBACK (Tie_column, after_line_breaking, 1);
SCM
Tie_column::after_line_breaking (SCM smob)
{
  set_directions (unsmob_grob (smob));
  return SCM_UNSPECIFIED;
}

/*
  Extend the spanner over its Tie constituents.
*/
MAKE_SCHEME_CALLBACK (Tie_column, before_line_breaking, 1);
SCM
Tie_column::before_line_breaking (SCM smob)
{
  Spanner *me = dynamic_cast<Spanner *> (unsmob_grob (smob));
  for (SCM s = me->get_property ("ties"); scm_is_pair (s); s = scm_cdr (s))
    {
      Spanner *tie = dynamic_cast<Spanner *> (unsmob_grob (scm_car (s)));
      Direction dir = LEFT;
      do
	{
	  if (dir * tie->get_bound (dir)->get_column ()->get_rank ()
	      > dir * me->get_bound (dir)->get_column ()->get_rank ())
	    me->set_bound (dir, Tie::head (tie, dir));
	}
      while (flip (&dir) != LEFT);
    }
  return SCM_UNSPECIFIED;
}

ADD_INTERFACE (Tie_column, "tie-column-interface",
	       "Object that sets directions of multiple ties in a tied chord",
	       "direction "
	       "positioning-done "
	       );




bool
config_allowed (map<Tie_configuration, bool> const &allowed,
		Tie_configuration conf)
{
  return allowed.find (conf) == allowed.end ();
}

void
add_configuration (map<Tie_configuration, bool> *allowed,
		   Grob *tie_column,
		   Tie_configuration new_conf)
{
  bool on_line = Staff_symbol_referencer::on_staffline (tie_column, new_conf.position_);
  
  if (allowed->find (new_conf) != allowed->end ()
      && !(*allowed)[new_conf])
    {
      programming_error ("Tie configuration not allowed");
    }
        

  if (on_line)
    {
      Tie_configuration forbidden;

      forbidden.dir_ = -new_conf.dir_ ;
      forbidden.position_ = new_conf.position_;
      (*allowed)[forbidden] = false;

      forbidden.position_ += new_conf.dir_;
      (*allowed)[forbidden] = false;
      forbidden.position_ += new_conf.dir_;
      (*allowed)[forbidden] = false;

      forbidden.dir_ = new_conf.dir_;
      forbidden.position_ = new_conf.position_ + new_conf.dir_;
      (*allowed)[forbidden] = false;
    }
  else
    {
      Tie_configuration forbidden;
      forbidden.dir_ = - new_conf.dir_;
      forbidden.position_ = new_conf.position_;

      
      (*allowed)[forbidden] = false;
      forbidden.position_ -= new_conf.dir_;
      forbidden.dir_ = new_conf.dir_;
      (*allowed)[forbidden] = false;

      forbidden.position_ += 2* new_conf.dir_; 
      (*allowed)[forbidden] = false;
    }
}


void
Tie_column::new_directions (Grob *me)
{
  extract_grob_set (me, "ties", ro_ties);
  Link_array<Grob> ties (ro_ties);
  if (!ties.size ())
    return;

  if (ties.size() == 1)
    {
      Tie::set_default_control_points (ties[0]);
      return ;
    }
  
  ties.sort (&Tie::compare);
  Array<Tie_configuration> tie_configs;
  for (int i = 0; i < ties.size (); i++)
    {
      Tie_configuration conf;
      conf.dir_ = get_grob_direction (ties[i]);
      conf.position_ = (int) rint (Tie::get_position (ties[i]));
      tie_configs.push (conf);
    }

    
  if (!tie_configs[0].dir_)
    tie_configs[0].dir_ = DOWN;
  if (!tie_configs.top().dir_)
    tie_configs.top().dir_ = UP;


  /*
    Seconds
   */
  for (int i = 1; i < tie_configs.size(); i++)
    {
      if (fabs (tie_configs[i-1].position_ - tie_configs[i].position_) <= 1)
	{
	  if (!tie_configs[i-1].dir_)
	    tie_configs[i-1].dir_ = DOWN;
	  if (!tie_configs[i].dir_)
	    tie_configs[i].dir_ = UP;
	}
    }

  for (int i = 1; i < tie_configs.size() - 1; i++)
    {
      if (tie_configs[i].dir_)
	continue;

      tie_configs[i].dir_ = (Direction) sign (tie_configs[i].position_);
    }

  Grob *common[NO_AXES] = {
    me, me
  };
  for (int i = 0; i < ties.size (); i++)
    for (int a = X_AXIS; a < NO_AXES; a++)
      {
	Axis ax ((Axis) a);
	
	common[ax] = dynamic_cast<Spanner*> (ties[i])->get_bound (LEFT)->common_refpoint (common[a], ax); 
	common[ax] = dynamic_cast<Spanner*> (ties[i])->get_bound (RIGHT)->common_refpoint (common[a], ax); 
      }
  
  map<Tie_configuration, bool> allowed;

  Tie::get_configuration (ties[0], common, &tie_configs.elem_ref (0));
  Tie::get_configuration (ties.top (), common,
			  &tie_configs.elem_ref (tie_configs.size()-1));

  add_configuration (&allowed, me, tie_configs[0]);
  add_configuration (&allowed, me, tie_configs.top());

  for (int i = 1; i < ties.size(); i++)
    {
      Tie_configuration conf = tie_configs[i];
      Tie::get_configuration (ties[i], common, &conf);
      if (!config_allowed (allowed, conf))
	{
	  conf = tie_configs[i];

	  Direction d = LEFT;
	  do
	    {
	      conf.attachment_x_[d] = d * 1e6; //  infty
	      for (int j = i - 1; j < i + 2; j++)
		{
		  if (j >= 0 && j < ties.size())
		    {
		      Spanner *t = dynamic_cast<Spanner*> (ties[j]);
		      Interval ext
			= robust_relative_extent (t->get_bound (d),
						  common[X_AXIS], X_AXIS);
		      conf.attachment_x_[d]
			= d * min (d * conf.attachment_x_[d], d * ext[-d]);
		    } 
		}
	    }
	  while (flip (&d) != LEFT);
	  tie_configs[i] = conf;
	}
      else
	tie_configs[i] = conf;

      add_configuration (&allowed, me, conf);
    }

  for (int i = 0; i < ties.size(); i++)
    {
      Tie::set_control_points (ties[i], common, tie_configs[i]);
      set_grob_direction (ties[i], tie_configs[i].dir_);
    }
}


