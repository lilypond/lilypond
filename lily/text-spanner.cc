/*

text-spanner.cc -- implement Text_spanner

source file of the GNU LilyPond music typesetter

(c) 2000--2002 Jan Nieuwenhuizen <janneke@gnu.org>
*/

#include "molecule.hh"
#include "text-item.hh"
#include "text-spanner.hh"
#include "line-spanner.hh"
#include "spanner.hh"
#include "font-interface.hh"
#include "dimensions.hh"
#include "paper-def.hh"
#include "debug.hh"
#include "paper-column.hh"
#include "staff-symbol-referencer.hh"

/*
  TODO:
  - vertical start / vertical end (fixme-name) |
  - contination types (vert. star, vert. end)  |-> eat volta-bracket
  - more styles
  - more texts/positions
*/

MAKE_SCHEME_CALLBACK (Text_spanner, brew_molecule, 1);

/*
  TODO: this function is too long, too hairy.

  TODO: document this. What the heck is happening here?
*/
SCM
Text_spanner::brew_molecule (SCM smob) 
{
  Grob *me= unsmob_grob (smob);
  Spanner *spanner = dynamic_cast<Spanner*> (me);

  if (spanner->has_interface (ly_symbol2scm ("piano-pedal-interface")) ) 
    {
      setup_pedal_bracket(spanner);
    }


  /* Ugh, must be same as Hairpin::brew_molecule.  */
  Real padding = gh_scm2double (me->get_grob_property ("if-text-padding"));
  Real broken_left =  spanner->get_broken_left_end_align ();
  Real width = spanner->spanner_length ();
  width -= broken_left;

  Drul_array<bool> broken;
  Drul_array<Real> extra_off;
  Direction d = LEFT;
  do
    {
      extra_off [d]=0;
      Item *b = spanner->get_bound (d);
      broken[d] = b->break_status_dir () != CENTER;

      if (!broken [d])
	{

	  Interval e = b->extent (b, X_AXIS);
	  Real r = 0.0;
	  if (!e.empty_b ())
	    r = e[-d] + padding;
	  /* Text spanners such as ottava, should span from outer limits of
	     noteheads, iso (de)cresc. spanners that span the inner space */
	  if (me->get_grob_property ("outer") != SCM_EOL)
	    // r *= -1; // huh?
	    {
	      width -= d * r;
	    }
	  else
	    {
	      width += d * r;
	      extra_off[d] = r;
	    }
	}
    }
  while (flip (&d) != LEFT);

  // FIXME: ecs tells us -- only for (de)cresc. spanners
  width += gh_scm2double (me->get_grob_property ("width-correct"));
  /* /Ugh */

  // who is ecs? --hwn

  SCM properties = Font_interface::font_alist_chain (me);

  SCM edge_text = me->get_grob_property ("edge-text");
  Drul_array<Molecule> edge;
  if (gh_pair_p (edge_text))
    {
      Direction d = LEFT;
      do
	{
	  /*  Don't repeat edge text for broken end */
	  if (!broken[d])
	    {
	      SCM text = index_cell (edge_text, d);
	      edge[d] = Text_item::text2molecule (me, text, properties);
	      if (!edge[d].empty_b ())
		edge[d].align_to (Y_AXIS, CENTER);
	    }
	}
      while (flip (&d) != LEFT);
    }
  width -= edge[LEFT].extent (X_AXIS).length ()
    + edge[RIGHT].extent (X_AXIS).length ();

  Drul_array<Real> shorten;
  shorten[LEFT] = 0;
  shorten[RIGHT] = 0;

  SCM s = me->get_grob_property ("shorten-pair");
  if (gh_pair_p (s))
    {
      shorten[LEFT] = gh_scm2double (ly_car (s));
      shorten[RIGHT] = gh_scm2double (ly_cdr (s));
    }

  width -= shorten[LEFT] + shorten[RIGHT];
  
  if (width < 0)
    {
      me->warning (_ ("Text_spanner too small"));
      width = 0;
    }

  /* ugh */
  
  Real thick = me->paper_l ()->get_var ("linethickness");  
  SCM st = me->get_grob_property ("thickness");
  if (gh_number_p (st))
    {
      thick *=  gh_scm2double (st);

    }
  Molecule line = Line_spanner::line_molecule (me, thick, width, 0);
  
  Drul_array<Molecule> edge_line;
  s = me->get_grob_property ("edge-height");
  SCM ew = me->get_grob_property ("edge-width");
  if (gh_pair_p (s))
    {
      Direction d = LEFT;
      int dir = to_dir (me->get_grob_property ("direction"));
      do
	{
	  Real dx = ( gh_pair_p (ew)  ? 
		      gh_scm2double (index_cell (ew, d)) * - dir  :  
		      0 );
	  Real dy = gh_scm2double (index_cell (s, d)) * - dir;
	  if (dy)
	    {
	      edge_line[d] = Line_spanner::line_molecule (me, thick, dx, dy);
	    }
	}
      while (flip (&d) != LEFT);
    }
  
  Molecule m;
  if (!edge[LEFT].empty_b ())
    m = edge[LEFT];

  if (!edge_line[LEFT].empty_b ())
    m.add_at_edge (X_AXIS, RIGHT, edge_line[LEFT], 0);
  if (!line.empty_b ())
    m.add_at_edge (X_AXIS, RIGHT, line,
		   edge_line[LEFT].empty_b () ? 0 : - thick/2);
  if (!edge_line[RIGHT].empty_b ())
    m.add_at_edge (X_AXIS, RIGHT, edge_line[RIGHT], -thick/2);
  if (!edge[RIGHT].empty_b ())
    m.add_at_edge (X_AXIS, RIGHT, edge[RIGHT], 0);
  m.translate_axis (broken_left + extra_off[LEFT] + shorten[LEFT], X_AXIS);

  return m.smobbed_copy ();
}



/* 
   Piano pedal brackets are a special case of a text spanner.
   Pedal up-down (restart) indicated by the angled right and left edges 
   of consecutive pedals touching exactly to form an __/\__
   Chris Jackson <chris@fluffhouse.org.uk>
*/

void 
Text_spanner::setup_pedal_bracket(Spanner *me)
{

  Real thick = me->paper_l ()->get_var ("linethickness");  
  SCM st = me->get_grob_property ("thickness");
  if (gh_number_p (st))
    {
      thick *=  gh_scm2double (st);
    }  

  Drul_array<bool> w, broken;
  Drul_array<Real> height, width, shorten, r;

  SCM pa = me->get_grob_property ("if-text-padding");
  SCM ew = me->get_grob_property ("edge-width");
  SCM eh = me->get_grob_property ("edge-height");
  SCM sp = me->get_grob_property ("shorten-pair");
  SCM wl = me->get_grob_property ("left-widen");
  SCM wr = me->get_grob_property ("right-widen");

  // Pedal has an angled left edge \__  or an angled right edge __/ 
  w[LEFT] = w[RIGHT] = false;
  if (gh_boolean_p (wl) )  
    w[LEFT]   = to_boolean (wl);
  if (gh_boolean_p (wr) )  
    w[RIGHT]  = to_boolean (wr);
  
  Direction d = LEFT;
  Interval e;
  Real padding = 0;

  if (gh_number_p (pa) )
    padding = gh_scm2double (pa);

  do {
    Item *b = me->get_bound (d);

    e = b->extent (b, X_AXIS);
    if (!e.empty_b ())
      r[d] = d * (e[-d] + padding);

    broken[d] = b->break_status_dir () != CENTER;
    width[d]  =  0;
    height[d] =  0;
    shorten[d] = 0;
    if ( w[d] && gh_pair_p (ew) )
      width[d] +=  gh_scm2double (index_cell (ew, d)) * d;
    if ( !broken[d] && (gh_pair_p (eh) ) )
      height[d] = gh_scm2double (index_cell (eh, d));
    if ( gh_pair_p (sp) )
      shorten[d] =  gh_scm2double (index_cell (sp, d));
  }
  while (flip (&d) != LEFT);
  
  Real extra_short = 0;
  // For 'Mixed' style pedals, i.e.  a bracket preceded by text:  Ped._____|
  // need to shorten by the extent of the text grob
  if ( to_boolean (me->get_grob_property ("text-start")) )
    {
      height[LEFT] = 0;
      Grob * textbit = me->get_parent(Y_AXIS);
      extra_short = padding;
      if (textbit->has_interface(ly_symbol2scm("piano-pedal-interface")))
	// for pretty Ped. scripts. 
	{
	  e = textbit->extent(textbit, Y_AXIS);
	  extra_short += e.length();
	}
      if (textbit->has_interface(ly_symbol2scm("text-interface"))) 
	// for plain text, e.g., Sost. Ped.
	{
	  SCM text  =  textbit->get_grob_property("text"); 
	  if (gh_string_p (text)) {
	    SCM properties = Font_interface::font_alist_chain (me);
	    Molecule mol = Text_item::text2molecule (me, text, properties);
	    extra_short += mol.extent(X_AXIS).length() / 2;
	  }
	}
      shorten[RIGHT] -= thick;
    }

  // Shorten a \____ on the left so that it will touch an adjoining ___/ 
  shorten[LEFT] += abs(width[LEFT]) * 2   +  extra_short ;
  
  if (broken[LEFT]) {
    shorten[LEFT] -= me->get_broken_left_end_align () ;
    shorten[RIGHT] -= r[RIGHT];
  }
  else 
    // Shorten bracket on the right so it ends just before the spanned note.
    shorten[RIGHT]  +=  thick  -  (r[LEFT]  +  r[RIGHT]);

  me->set_grob_property ("edge-height", gh_cons ( gh_double2scm ( height[LEFT] ) , 
						  gh_double2scm ( height[RIGHT]) ) );
  me->set_grob_property ("edge-width",  gh_cons ( gh_double2scm ( width[LEFT]  ), 
						  gh_double2scm ( width[RIGHT] ) ));
  me->set_grob_property ("shorten-pair", gh_cons ( gh_double2scm ( shorten[LEFT] ), 
						   gh_double2scm ( shorten[RIGHT] ) ));
}



ADD_INTERFACE (Pianopedal,"piano-pedal-interface",
  "",
  "pedal-type edge-width edge-height shorten-pair text-start left-widen right-widen");



ADD_INTERFACE (Text_spanner,"text-spanner-interface",
  "generic text spanner",
  "dash-period dash-length edge-height edge-width edge-text shorten-pair type");

