/*
  stem.cc -- implement Stem

  source file of the GNU LilyPond music typesetter

  (c) 1996--2002 Han-Wen Nienhuys <hanwen@cs.uu.nl>
    Jan Nieuwenhuizen <janneke@gnu.org>

  TODO: This is way too hairy

  TODO: fix naming.

  Stem-end, chord-start, etc. is all confusing naming.
*/

#include <math.h>		// rint

#include "lookup.hh"
#include "directional-element-interface.hh"
#include "note-head.hh"
#include "stem.hh"
#include "warn.hh"
#include "paper-def.hh"
#include "rhythmic-head.hh"
#include "font-interface.hh"
#include "molecule.hh"
#include "paper-column.hh"
#include "misc.hh"
#include "beam.hh"
#include "rest.hh"
#include "group-interface.hh"
#include "staff-symbol-referencer.hh"
#include "spanner.hh"
#include "side-position-interface.hh"
#include "dot-column.hh"

void
Stem::set_beaming (Grob*me, int beam_count,  Direction d)
{
  SCM pair = me->get_grob_property ("beaming");
  
  if (!gh_pair_p (pair))
    {
      pair = gh_cons (SCM_EOL, SCM_EOL);
      me->set_grob_property ("beaming", pair);
    }

  SCM l = index_get_cell (pair, d);
  for( int i = 0; i<  beam_count; i++)
    {
      l = gh_cons (gh_int2scm (i), l);
    }
  index_set_cell (pair, d, l);		
}


Interval
Stem::head_positions (Grob*me) 
{
  if (!head_count (me))
    {
      Interval iv;
      return iv;
    }

  Drul_array<Grob*> e (extremal_heads (me));

  return Interval (Staff_symbol_referencer::get_position (e[DOWN]),
		   Staff_symbol_referencer::get_position (e[UP]));
}


Real
Stem::chord_start_y (Grob*me) 
{
  return head_positions (me)[get_direction (me)]
    * Staff_symbol_referencer::staff_space (me)/2.0;
}

Real
Stem::stem_end_position (Grob*me) 
{
  SCM p =me->get_grob_property ("stem-end-position");
  Real pos;
  if (!gh_number_p (p))
    {
      pos = get_default_stem_end_position (me);
      me->set_grob_property ("stem-end-position", gh_double2scm (pos));
    }
  else
    pos = gh_scm2double (p);

  return pos;
}

Direction
Stem::get_direction (Grob*me)
{
  Direction d = Directional_element_interface::get (me);

  if (!d)
    {
       d = get_default_dir (me);
       // urg, AAARGH!
       Directional_element_interface::set (me, d);
    }
  return d ;
}


void
Stem::set_stemend (Grob*me, Real se)
{
  // todo: margins
  Direction d= get_direction (me);
  
  if (d && d * head_positions (me)[get_direction (me)] >= se*d)
    me->warning (_ ("Weird stem size; check for narrow beams"));

  me->set_grob_property ("stem-end-position", gh_double2scm (se));
}


/*
  Note head that determines hshift for upstems

  WARNING: triggers direction
*/ 
Grob*
Stem::support_head (Grob*me)
{
  SCM h = me->get_grob_property ("support-head");
  Grob * nh = unsmob_grob (h);
  if (nh)
    return nh;
  else if (head_count (me) == 1)
    {
      /*
	UGH.
       */
      
      return unsmob_grob (ly_car (me->get_grob_property ("note-heads")));
    }
  else
    return first_head (me);
}


int
Stem::head_count (Grob*me)
{
  return  Pointer_group_interface::count (me, "note-heads");
}

/*
  The note head which forms one end of the stem.  

  WARNING: triggers direction
*/
Grob*
Stem::first_head (Grob*me)
{
  Direction d = get_direction (me);
  if (!d)
    return 0;
  return extremal_heads (me)[-d];
}

/*
  The note head opposite to the first head.
 */
Grob*
Stem::last_head (Grob*me)
{
  Direction d = get_direction (me);
  if (!d)
    return 0;
  return extremal_heads (me)[d];
}

/*
  START is part where stem reaches `last' head. 
 */
Drul_array<Grob*>
Stem::extremal_heads (Grob*me) 
{
  const int inf = 1000000;
  Drul_array<int> extpos;
  extpos[DOWN] = inf;
  extpos[UP] = -inf;  
  
  Drul_array<Grob *> exthead;
  exthead[LEFT] = exthead[RIGHT] =0;
  
  for (SCM s = me->get_grob_property ("note-heads"); gh_pair_p (s); s = ly_cdr (s))
    {
      Grob * n = unsmob_grob (ly_car (s));

      
      int p = int (Staff_symbol_referencer::get_position (n));

      Direction d = LEFT;
      do {
      if (d* p > d* extpos[d])
	{
	  exthead[d] = n;
	  extpos[d] = p;
	}
      } while (flip (&d) != DOWN);
    }

  return exthead;
}

static int
icmp (int const &a, int const &b)
{
  return a-b;
}

Array<int>
Stem::note_head_positions (Grob *me)
{
  Array<int> ps ;
  for (SCM s = me->get_grob_property ("note-heads"); gh_pair_p (s); s = ly_cdr (s))
    {
      Grob * n = unsmob_grob (ly_car (s));
      int p = int (Staff_symbol_referencer::get_position (n));

      ps.push (p);
    }

  ps.sort (icmp);
  return ps; 
}


void
Stem::add_head (Grob*me, Grob *n)
{
  n->set_grob_property ("stem", me->self_scm ());
  n->add_dependency (me);

  if (Note_head::has_interface (n))
    {
      Pointer_group_interface::add_grob (me, ly_symbol2scm ("note-heads"), n);
    }
}

bool
Stem::invisible_b (Grob*me)
{
  return ! (head_count (me)
	    && gh_scm2int (me->get_grob_property ("duration-log")) >= 1);
}

Direction
Stem::get_default_dir (Grob*me) 
{
  int staff_center = 0;
  Interval hp = head_positions (me);
  if (hp.empty_b())
    {
      return CENTER;
    }
  
  int udistance = (int) (UP * hp[UP] - staff_center);
  int ddistance = (int) (DOWN* hp[DOWN] - staff_center);  
  
  if (sign (ddistance - udistance))
    return Direction (sign (ddistance -udistance));

  return to_dir (me->get_grob_property ("neutral-direction"));
}

Real
Stem::get_default_stem_end_position (Grob*me) 
{
  /* Tab notation feature: make stem end extend out of staff. */
  SCM up_to_staff = me->get_grob_property ("up-to-staff");
  if (to_boolean (up_to_staff))
    {
      int line_count = Staff_symbol_referencer::line_count (me);
      Direction dir = get_direction (me);
      return dir * (line_count + 3.5);
    }
  
  bool grace_b = to_boolean (me->get_grob_property ("grace"));
  SCM s;
  Array<Real> a;

  Real length_f = 3.5;
  SCM scm_len = me->get_grob_property ("length");
  if (gh_number_p (scm_len))
    {
      length_f = gh_scm2double (scm_len);
    }
  else
    {
      s = me->get_grob_property ("lengths");
      if (gh_pair_p (s))
	{
	  length_f = 2* gh_scm2double (robust_list_ref (duration_log(me) -2, s));
	}
    }

  Real shorten_f = 0.0;
  
  SCM sshorten = me->get_grob_property ("stem-shorten");
  if (gh_pair_p (sshorten))
    {
      shorten_f = 2* gh_scm2double (robust_list_ref ((duration_log (me) - 2) >? 0, sshorten));
    }

  /* On boundary: shorten only half */
  if (abs (chord_start_y (me)) == 0.5)
    shorten_f *= 0.5;

  /* URGURGURG
     'set-default-stemlen' sets direction too
   */
  Direction dir = get_direction (me);
  if (!dir)
    {
      dir = get_default_dir (me);
      Directional_element_interface::set (me, dir);
    }
  
  /* stems in unnatural (forced) direction should be shortened, 
    according to [Roush & Gourlay] */
  if (chord_start_y (me)
      && (get_direction (me) != get_default_dir (me)))
    length_f -= shorten_f;

  Interval hp = head_positions (me);  
  Real st = hp[dir] + dir * length_f;


  /*
    TODO: change name  to extend-stems to staff/center/'()
  */
  bool no_extend_b = to_boolean (me->get_grob_property ("no-stem-extend"));
  if (!grace_b && !no_extend_b && dir * st < 0) // junkme?
    st = 0.0;

  /*
    Make a little room if we have a upflag and there is a dot.
    previous approach was to lengthen the stem. This is not
    good typesetting practice. 
    
  */
  if (!get_beam (me) && dir == UP
      && duration_log (me) > 2)
    {
      Grob * closest_to_flag = extremal_heads (me)[dir];
      Grob * dots = closest_to_flag
	? Rhythmic_head::get_dots (closest_to_flag ) : 0;

      if (dots)
	{
	  Real dp = Staff_symbol_referencer::get_position (dots);
	  Real flagy =  flag (me).extent (Y_AXIS)[-dir] * 2
	    / Staff_symbol_referencer::staff_space (me); 

	  /*
	    Very gory: add myself to the X-support of the parent,
	    which should be a dot-column.
	   */
	  if (dir * (st + flagy -  dp) < 0.5)
	    {
	      Grob *par = dots->get_parent (X_AXIS);

	      if (Dot_column::has_interface (par))
		{
		  Side_position_interface::add_support (par, me);

		  /*
		    TODO: apply some better logic here. The flag is
		    curved inwards, so this will typically be too
		    much.
		  */
		}
	    }
	}
    }


  return st;
}



/*
  
  the log of the duration (Number of hooks on the flag minus two)
 */
int
Stem::duration_log (Grob*me) 
{
  SCM s = me->get_grob_property ("duration-log");
  return (gh_number_p (s)) ? gh_scm2int (s) : 2;
}

void
Stem::position_noteheads (Grob*me)
{
  if (!head_count (me))
    return;
  
  Link_array<Grob> heads =
    Pointer_group_interface__extract_grobs (me, (Grob*)0, "note-heads");

  heads.sort (compare_position);
  Direction dir =get_direction (me);
  
  if (dir < 0)
    heads.reverse ();


  bool invisible = invisible_b (me);
  Real thick = 0.0;
  if (invisible)
        thick = gh_scm2double (me->get_grob_property ("thickness"))
	  * me->get_paper ()->get_var ("linethickness");
      

  Grob *hed = support_head (me);
  Real w = Note_head::head_extent (hed,X_AXIS)[dir];
  for (int i=0; i < heads.size (); i++)
    {
      heads[i]->translate_axis (w - Note_head::head_extent (heads[i],X_AXIS)[dir],
				X_AXIS);
    }
  
  bool parity= true;		// todo: make me settable.
  int lastpos = int (Staff_symbol_referencer::get_position (heads[0]));
  for (int i=1; i < heads.size (); i ++)
    {
      Real p = Staff_symbol_referencer::get_position (heads[i]);
      int dy =abs (lastpos- (int)p);

      if (dy <= 1)
	{
	  if (parity)
	    {
	      Real l = Note_head::head_extent (heads[i], X_AXIS).length ();

	      Direction d = get_direction (me);
	      heads[i]->translate_axis (l * d, X_AXIS);

	      if (invisible_b(me))
		heads[i]->translate_axis (-thick *2* d , X_AXIS);

	      
	     /* TODO:
		 
	      For some cases we should kern some more: when the
	      distance between the next or prev note is too large, we'd 
	      get large white gaps, eg.
	      
               |
              X|
	       |X  <- kern this.
	       |
	      X
	      
	      */
	    }
	  parity = !parity;
	}
      else
	parity = true;
      
      lastpos = int (p);
    }
}

MAKE_SCHEME_CALLBACK (Stem,before_line_breaking,1);
SCM
Stem::before_line_breaking (SCM smob)
{
  Grob*me = unsmob_grob (smob);


  /*
    Do the calculations for visible stems, but also for invisible stems
    with note heads (i.e. half notes.)
   */
  if (head_count (me))
    {
      stem_end_position (me);	// ugh. Trigger direction calc.
      position_noteheads (me);
    }
  else
    {
      me->set_grob_property ("molecule-callback", SCM_EOL);
    }
  
  return SCM_UNSPECIFIED;
}

/*
  ugh.
  When in a beam with tuplet brackets, brew_mol is called early,
  caching a wrong value.
 */
MAKE_SCHEME_CALLBACK (Stem, height, 2);
SCM
Stem::height (SCM smob, SCM ax)
{
  Axis a = (Axis)gh_scm2int (ax);
  Grob * me = unsmob_grob (smob);
  assert (a == Y_AXIS);

  SCM mol = me->get_uncached_molecule ();
  Interval iv;
  if (mol != SCM_EOL)
    iv = unsmob_molecule (mol)->extent (a);
  if (Grob *b =get_beam (me))
    {
      Direction d = get_direction (me);
      iv[d] += d * Beam::get_thickness (b) /2.0 ;
    }

  return ly_interval2scm (iv);
}


Molecule
Stem::flag (Grob*me)
{
  /* TODO: rename flag-style into something more appropriate,
   e.g. "stroke-style", maybe with values "" (i.e. no stroke),
   "single" and "double".  Needs more discussion.
  */
  String style, staffline_offs;

  SCM style_scm = me->get_grob_property ("style");
  if (gh_symbol_p (style_scm))
    {
      style = (ly_scm2string (scm_symbol_to_string (style_scm)));
    }
  else
    {
      style = "";
    }
  bool adjust = to_boolean (me->get_grob_property ("adjust-if-on-staffline"));

  if (String::compare (style, "mensural") == 0)
    /* Mensural notation: For notes on staff lines, use different
       flags than for notes between staff lines.  The idea is that
       flags are always vertically aligned with the staff lines,
       regardless if the note head is on a staff line or between two
       staff lines.  In other words, the inner end of a flag always
       touches a staff line.
    */
    {
      if (adjust)
        {
	  /* Urrgh!  We have to detect wether this stem ends on a staff
	     line or between two staff lines.  But we can not call
	     stem_end_position(me) or get_default_stem_end_position(me),
	     since this encounters the flag and hence results in an
	     infinite recursion.  However, in pure mensural notation,
	     there are no multiple note heads attached to a single stem,
	     neither is there usually need for using the stem_shorten
	     property (except for 32th and 64th notes, but that is not a
	     problem since the stem length in this case is augmented by
	     an integral multiple of staff_space).  Hence, it should be
	     sufficient to just take the first note head, assume it's
	     the only one, look if it's on a staff line, and select the
	     flag's shape accordingly.  In the worst case, the shape
	     looks slightly misplaced, but that will usually be the
	     programmer's fault (e.g. when trying to attach multiple
	     note heads to a single stem in mensural notation).  */

	  /*
	    perhaps the detection whether this correction is needed should
	    happen in a different place  to avoid the recursion.
	    
	    --hwn.
	  */
	  int p = (int)rint (Staff_symbol_referencer::get_position (first_head (me)));
	  staffline_offs = Staff_symbol_referencer::on_staffline (me, p) ?
	    "1" : "0";
	}
      else
        {
	  staffline_offs = "2";
	}
    }
  else
    {
      staffline_offs = "";
    }
  char dir = (get_direction (me) == UP) ? 'u' : 'd';
  String font_char =
    style + to_string (dir) + staffline_offs + to_string (duration_log (me));
  Font_metric *fm = Font_interface::get_default_font (me);
  Molecule flag = fm->find_by_name ("flags-" + font_char);
  if (flag.empty_b ())
    {
      me->warning (_f ("flag `%s' not found", font_char));
    }

  SCM stroke_scm = me->get_grob_property ("flag-style");
  if (gh_string_p (stroke_scm))
    {
      String stroke = ly_scm2string (stroke_scm);
      if (!stroke.empty_b ())
	{
	  String font_char = to_string (dir) + stroke;
	  Molecule stroke = fm->find_by_name ("flags-" + font_char);
	  if (stroke.empty_b ())
	    {
	      me->warning (_f ("flag stroke `%s' not found", font_char));
	    }
	  else
	    {
	      flag.add_molecule (stroke);
	    }
	}
    }

  return flag;
}

MAKE_SCHEME_CALLBACK (Stem,dim_callback,2);
SCM
Stem::dim_callback (SCM e, SCM ax)
{
  Axis a = (Axis) gh_scm2int (ax);
  assert (a == X_AXIS);
  Grob *se = unsmob_grob (e);
  Interval r (0, 0);
  if (unsmob_grob (se->get_grob_property ("beam")) || abs (duration_log (se)) <= 2)
    ;	// TODO!
  else
    {
      r = flag (se).extent (X_AXIS);
    }
  return ly_interval2scm (r);
}
 


MAKE_SCHEME_CALLBACK (Stem,brew_molecule,1);

SCM
Stem::brew_molecule (SCM smob) 
{
  Grob*me = unsmob_grob (smob);
  Molecule mol;
  Direction d = get_direction (me);
  
  
     
  Real y1;

  /*
    This is required to avoid stems passing in tablature chords...
   */


  /*
    TODO: make  the stem start a direction ?
  */
  

  
  if (to_boolean (me->get_grob_property ("avoid-note-head")))
    {
      Grob * lh = last_head (me);
      if (!lh)
	return SCM_EOL;
      y1 = Staff_symbol_referencer::get_position (lh);
    }
  else
    {
      Grob * lh = first_head (me);
      if (!lh)
	return SCM_EOL;
      y1 = Staff_symbol_referencer::get_position (lh);
    }
  
  Real y2 = stem_end_position (me);
  
  Interval stem_y (y1 <? y2,y2 >? y1);


  // dy?
  Real dy = Staff_symbol_referencer::staff_space (me) * 0.5;

  if (Grob *hed = support_head (me))
    {
      /*
	must not take ledgers into account.
       */
      Interval head_height = Note_head::head_extent (hed,Y_AXIS);
      Real y_attach = Note_head::stem_attachment_coordinate ( hed, Y_AXIS);

      y_attach = head_height.linear_combination (y_attach);
      stem_y[Direction (-d)] += d * y_attach/dy;
    }
  
  if (!invisible_b (me))
    {
      Real stem_width = gh_scm2double (me->get_grob_property ("thickness"))
	// URG
	* me->get_paper ()->get_var ("linethickness");
      
      Molecule ss =Lookup::filledbox (Box (Interval (-stem_width/2, stem_width/2),
					   Interval (stem_y[DOWN]*dy, stem_y[UP]*dy)));
      mol.add_molecule (ss);
    }

  if (!get_beam (me) && abs (duration_log (me)) > 2)
    {
      Molecule fl = flag (me);
      fl.translate_axis (stem_y[d]*dy, Y_AXIS);
      mol.add_molecule (fl);
    }

  return mol.smobbed_copy ();
}

/*
  move the stem to right of the notehead if it is up.
 */
MAKE_SCHEME_CALLBACK (Stem,off_callback,2);
SCM
Stem::off_callback (SCM element_smob, SCM)
{
  Grob *me = unsmob_grob (element_smob);
  
  Real r=0;

  if (head_count (me) == 0)
    {
      return gh_double2scm (0.0);
    }
  
  if (Grob * f = first_head (me))
    {
      Interval head_wid = Note_head::head_extent(f, X_AXIS);

      
      Real attach =0.0;

      if (invisible_b (me))
	{
	  attach = 0.0;
	}
      else
	attach = Note_head::stem_attachment_coordinate(f, X_AXIS);

      Direction d = get_direction (me);

      Real real_attach = head_wid.linear_combination (d * attach);

      r = real_attach;

      /*
	If not centered: correct for stem thickness.
       */
      if (attach)
	{
	  Real rule_thick
	    = gh_scm2double (me->get_grob_property ("thickness"))
	    * me->get_paper ()->get_var ("linethickness");

	  
	  r += - d * rule_thick * 0.5;
	}
    }
  return gh_double2scm (r);
}

Grob*
Stem::get_beam (Grob*me)
{
  SCM b=  me->get_grob_property ("beam");
  return unsmob_grob (b);
}

Stem_info
Stem::get_stem_info (Grob *me)
{
  /* Return cached info if available */
  SCM scm_info = me->get_grob_property ("stem-info");
  if (!gh_pair_p (scm_info))
    {
      calc_stem_info (me);
      scm_info = me->get_grob_property ("stem-info");
    }
  
  Stem_info si;
  si.dir_ = Directional_element_interface::get (me); 
  si.ideal_y_ = gh_scm2double (gh_car (scm_info)); 
  si.shortest_y_ = gh_scm2double (gh_cadr (scm_info));
  return si;
}

void
Stem::calc_stem_info (Grob *me)
{
  /* Tab notation feature: make stem end extend out of staff. */
  SCM up_to_staff = me->get_grob_property ("up-to-staff");
  if (to_boolean (up_to_staff))
    {
      int line_count = Staff_symbol_referencer::line_count (me);
      Direction dir = get_direction (me);
      Real ideal_y = dir * (line_count + 1.5);
      Real shortest_y = ideal_y;
      
      me->set_grob_property ("stem-info",
			     scm_list_n (gh_double2scm (ideal_y),
					 gh_double2scm (shortest_y),
					 SCM_UNDEFINED));
      return;
    }

  Direction my_dir = Directional_element_interface::get (me);
  Real staff_space = Staff_symbol_referencer::staff_space (me);
  Grob *beam = get_beam (me);
  Real beam_translation = Beam::get_beam_translation (beam);
  Real beam_thickness = gh_scm2double (beam->get_grob_property ("thickness"));
  int beam_count = Beam::get_direction_beam_count (beam, my_dir);


  /* Simple standard stem length */
  Real ideal_length =
    gh_scm2double (robust_list_ref
		   (beam_count - 1,
		    me->get_grob_property ("beamed-lengths")))
    * staff_space
    /* stem only extends to center of beam */
    - 0.5 * beam_thickness;

  
  /* Condition: sane minimum free stem length (chord to beams) */
  Real ideal_minimum_free =
    gh_scm2double (robust_list_ref
		   (beam_count - 1,
		    me->get_grob_property ("beamed-minimum-free-lengths")))
    * staff_space;
  

  /* UGH
     It seems that also for ideal minimum length, we must use
     the maximum beam count (for this direction):
     
     \score{ \notes\relative c''{ [a8 a32] }}
     
     must be horizontal. */
  Real height_of_my_beams = beam_thickness
    + (beam_count - 1) * beam_translation;

  Real ideal_minimum_length = ideal_minimum_free
    + height_of_my_beams
    /* stem only extends to center of beam */
    - 0.5 * beam_thickness;

  ideal_length = ideal_length >? ideal_minimum_length;

  
  /* Convert to Y position, calculate for dir == UP */
  Real note_start =
    /* staff positions */
    head_positions (me)[my_dir] * 0.5
    * my_dir;
  Real ideal_y = note_start + ideal_length;


  /* Conditions for Y position */

  /* Lowest beam of (UP) beam must never be lower than second staffline
 
     Reference?
 
     Although this (additional) rule is probably correct,
     I expect that highest beam (UP) should also never be lower
     than middle staffline, just as normal stems.

     Reference?

     Obviously not for grace beams.
     
     Also, not for knees.  Seems to be a good thing. */
  SCM grace = me->get_grob_property ("grace");
  bool grace_b = to_boolean (grace);
  bool no_extend_b = to_boolean (me->get_grob_property ("no-stem-extend"));
  bool knee_b = to_boolean (beam->get_grob_property ("knee"));
  if (!grace_b && !no_extend_b && !knee_b)
    {
      /* Highest beam of (UP) beam must never be lower than middle
	 staffline */
      ideal_y =	ideal_y >? 0;
      /* Lowest beam of (UP) beam must never be lower than second staffline */
      ideal_y =	ideal_y >? (-staff_space
			    - beam_thickness + height_of_my_beams);
    }


  SCM shorten = beam->get_grob_property ("shorten");
  if (gh_number_p (shorten))
    ideal_y -= gh_scm2double (shorten);


  Real minimum_free =
    gh_scm2double (robust_list_ref
		   (beam_count - 1,
		    me->get_grob_property
		    ("beamed-extreme-minimum-free-lengths")))
    * staff_space;

  Real minimum_length = minimum_free
    + height_of_my_beams
    /* stem only extends to center of beam */
    - 0.5 * beam_thickness;

  Real minimum_y = note_start + minimum_length;
  
  
  ideal_y *= my_dir;
  Real shortest_y = minimum_y * my_dir; 
  
  me->set_grob_property ("stem-info",
			 scm_list_n (gh_double2scm (ideal_y),
				     gh_double2scm (shortest_y),
				     SCM_UNDEFINED));
}

Slice
Stem::beam_multiplicity (Grob *stem)
{
  SCM beaming= stem->get_grob_property ("beaming");
  Slice l = int_list_to_slice (gh_car (beaming));
  Slice r = int_list_to_slice (gh_cdr (beaming));
  l.unite (r);

  return l;
}


ADD_INTERFACE (Stem,"stem-interface",
  "A stem",
  "up-to-staff avoid-note-head adjust-if-on-staffline thickness stem-info beamed-lengths beamed-minimum-free-lengths beamed-extreme-minimum-free-lengths lengths beam stem-shorten duration-log beaming neutral-direction stem-end-position support-head note-heads direction length style no-stem-extend flag-style");


