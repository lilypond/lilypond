/* 
  ledger-line-spanner.cc --  implement Ledger_line_spanner
  
  source file of the GNU LilyPond music typesetter
  
  (c) 2004 Han-Wen Nienhuys <hanwen@xs4all.nl>
  
*/

#include <map>

#include "item.hh" 
#include "note-head.hh" 
#include "staff-symbol-referencer.hh" 
#include "staff-symbol.hh" 
#include "lookup.hh" 
#include "spanner.hh" 
#include "group-interface.hh" 
#include "paper-column.hh"

struct Ledger_line_spanner
{
  DECLARE_SCHEME_CALLBACK (print, (SCM ));
  static Stencil brew_ledger_lines (Grob *me,
			     int pos,
			     int interspaces,
			     Interval x_extent,
			     Real left_shorten);

  static bool has_interface (Grob*);
};


Stencil
Ledger_line_spanner::brew_ledger_lines (Grob *me,
					  int pos,
					  int interspaces,
					  Interval x_extent,
					  Real left_shorten)
{
  Grob *staff = Staff_symbol_referencer::get_staff_symbol (me);
  Real inter_f = Staff_symbol_referencer::staff_space (me)/2;
  int line_count = ((abs (pos) < interspaces)
		    ? 0
		    : (abs (pos) - interspaces) / 2);
  Stencil stencil;
  if (line_count)
    {
      Real ledgerlinethickness =
	Staff_symbol::get_ledger_line_thickness (staff);
      Real blotdiameter = ledgerlinethickness;
      Interval y_extent =
	Interval (-0.5*(ledgerlinethickness),
		  +0.5*(ledgerlinethickness));
      Stencil proto_ledger_line =
	Lookup::round_filled_box (Box (x_extent, y_extent), blotdiameter);

      x_extent[LEFT] += left_shorten;
      Stencil proto_first_line =
	Lookup::round_filled_box (Box (x_extent, y_extent), blotdiameter);

      Direction dir = (Direction)sign (pos);
      Real offs = (Staff_symbol_referencer::on_staffline (me, pos))
        ? 0.0
        : -dir * inter_f;


      offs += pos * inter_f;
      for (int i = 0; i < line_count; i++)
        {
          Stencil ledger_line ((i == 0) 
				? proto_first_line
				: proto_ledger_line
				);
          ledger_line.translate_axis (-dir * inter_f * i * 2 + offs, Y_AXIS);
          stencil.add_stencil (ledger_line);
        }
    }

  return stencil;
}


struct Ledger_request
{
  Interval ledger_extent_;
  Interval head_extent_;
  int position_;
  bool excentric_;
  Ledger_request () {
    ledger_extent_.set_empty ();
    head_extent_.set_empty ();
    position_ = 0;
  }
};

typedef std::map<int, Drul_array<Ledger_request> > Ledger_requests;


/*
  TODO: ledger share a lot of info. Lots of room to optimize away common
  use of objects/variables.
 */
MAKE_SCHEME_CALLBACK (Ledger_line_spanner,print,1);
SCM
Ledger_line_spanner::print (SCM smob)
{
  Spanner *me = dynamic_cast<Spanner*> (unsmob_grob (smob));
  Link_array<Grob> heads (Pointer_group_interface__extract_grobs (me, (Grob*)0, "note-heads"));

  Stencil ledgers;
  Stencil default_ledger;

  Grob * common[NO_AXES];
  for (int i = X_AXIS;  i < NO_AXES; i++)
    common[Axis (i)] = common_refpoint_of_array (heads, me, Axis(i));

  int interspaces = Staff_symbol_referencer::line_count (me)-1;
  Ledger_requests reqs;
  Real length_fraction = 0.25  * 2;
  for (int i = heads.size (); i--; )
    {
      Item *h = dynamic_cast<Item*> (heads[i]);
      
      int pos = Staff_symbol_referencer::get_rounded_position (h);
      if (abs (pos) > interspaces + 1)
	{
	  Interval head_extent = h->extent (common[X_AXIS], X_AXIS);
	  Interval ledger_extent = Interval (head_extent.linear_combination (-1 - length_fraction),
					     head_extent.linear_combination (1 + length_fraction));

	  Direction vdir = Direction (sign (pos));
	  int rank = Paper_column::get_rank (h->get_column ());
	  
	  reqs[rank][vdir].ledger_extent_.unite (ledger_extent);
	  reqs[rank][vdir].head_extent_.unite (head_extent);
	  reqs[rank][vdir].position_ =
	    vdir * ((vdir* reqs[rank][vdir].position_) >? (vdir *pos));
	}
    }

  Real gap = robust_scm2double (me->get_property ("gap"), 0.15);
  Ledger_requests::iterator last (reqs.end ());
  for (Ledger_requests::iterator i (reqs.begin ());
       i != reqs.end (); last = i++)
    {
      if (last == reqs.end ())
	{
	  continue;
	}
      
      Direction d = DOWN;
      do
	{
	  if (abs (last->second[d].position_) > interspaces + 1
	      && abs (i->second[d].position_) > interspaces + 1)
	    {
	      Real center =  
		(last->second[d].head_extent_[RIGHT]
		 + i->second[d].head_extent_[LEFT] )/2;

	      Direction which = LEFT;
	      do
		{
		  Ledger_request &lr = ((which == LEFT) ? *last : *i).second[d];


		  // due tilt of quarter note-heads 
		  Real excentricity = 0; //.1;
		  Real limit = (center + which * gap/2 + excentricity);
		  lr.ledger_extent_.elem_ref (-which)
		    = which  * (which * lr.ledger_extent_[-which] >? which * limit);
		}
	      while (flip (&which) != LEFT); 
	    }
	}
      while (flip (&d) != DOWN); 
    }

  for (Ledger_requests::const_iterator i (reqs.begin ());
       i != reqs.end (); i++)
    {
      Direction d = DOWN;
      do
	{
	  Ledger_request lr = (*i).second[d];
	  ledgers.add_stencil (brew_ledger_lines (me,
						  lr.position_,
						  interspaces,
						  lr.ledger_extent_,
						  0.0));
	}
      while (flip (&d) != DOWN); 
    }

  ledgers.translate_axis (-me->relative_coordinate (common[X_AXIS], X_AXIS),
			  X_AXIS);
  
  return ledgers.smobbed_copy ();
}

ADD_INTERFACE (Ledger_line_spanner,
	       "ledger-line-interface",
	       "This spanner draws the ledger lines of a staff on note heads. ",
	       "note-heads thickness gap length minimum-length")
