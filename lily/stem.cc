/*
  stem.cc -- implement Stem

  source file of the GNU LilyPond music typesetter

  (c) 1996--2001 Han-Wen Nienhuys <hanwen@cs.uu.nl>
    Jan Nieuwenhuizen <janneke@gnu.org>

  TODO: This is way too hairy
*/
#include <math.h>		// m_pi

#include "lookup.hh"
#include "directional-element-interface.hh"
#include "note-head.hh"
#include "stem.hh"
#include "debug.hh"
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

void
Stem::set_beaming (Grob*me ,int i,  Direction d)
{
  SCM pair = me->get_grob_property ("beaming");
  
  if (!gh_pair_p (pair))
    {
      pair = gh_cons (gh_int2scm (0),gh_int2scm (0));
      me->      set_grob_property ("beaming", pair);
    }
  index_set_cell (pair, d, gh_int2scm (i));
}

int
Stem::beam_count (Grob*me,Direction d)
{
  SCM p=me->get_grob_property ("beaming");
  if (gh_pair_p (p))
    return gh_scm2int (index_cell (p,d));
  else
    return 0;
}

Interval
Stem::head_positions (Grob*me) 
{
  if (!heads_i (me))
    {
      Interval iv;
      return iv;
    }

  Drul_array<Grob*> e (extremal_heads (me));

  return Interval (Staff_symbol_referencer::position_f (e[DOWN]),
		   Staff_symbol_referencer::position_f (e[UP]));
}


Real
Stem::chord_start_f (Grob*me) 
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
    warning (_ ("Weird stem size; check for narrow beams"));

  me->set_grob_property ("stem-end-position", gh_double2scm (se));
}

int
Stem::type_i (Grob*me) 
{
  return first_head (me) ?  Rhythmic_head::balltype_i (first_head (me)) : 2;
}

/*
  Note head that determines hshift for upstems
 */ 
Grob*
Stem::support_head (Grob*me)
{
  SCM h = me->get_grob_property ("support-head");
  Grob * nh = unsmob_grob (h);
  if (nh)
    return nh;
  else if (heads_i (me) == 1)
    {
      /*
	UGH.
       */
      
      return unsmob_grob (gh_car (me->get_grob_property ("heads")));
    }
  else
    return first_head (me);
}


int
Stem::heads_i (Grob*me)
{
  return  Pointer_group_interface::count (me, "heads");
}

/*
  The note head which forms one end of the stem.  
 */
Grob*
Stem::first_head (Grob*me)
{
  return extremal_heads (me)[-get_direction (me)];
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
  
  for (SCM s = me->get_grob_property ("heads"); gh_pair_p (s); s = gh_cdr (s))
    {
      Grob * n = unsmob_grob (gh_car (s));

      
      int p = int (Staff_symbol_referencer::position_f (n));

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
  for (SCM s = me->get_grob_property ("heads"); gh_pair_p (s); s = gh_cdr (s))
    {
      Grob * n = unsmob_grob (gh_car (s));
      int p = int (Staff_symbol_referencer::position_f (n));

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
      Pointer_group_interface::add_element (me, "heads",n);
    }
  else
    {
      n->set_grob_property ("rest", n->self_scm ());
    }
}

bool
Stem::invisible_b (Grob*me)
{
  return ! (heads_i (me) && Rhythmic_head::balltype_i (support_head (me)) >= 1);
}

int
Stem::get_center_distance (Grob*me, Direction d)
{
  int staff_center = 0;
  int distance = (int) (d* (head_positions (me)[d] - staff_center));
  return distance >? 0;
}

Direction
Stem::get_default_dir (Grob*me) 
{
  int du = get_center_distance (me,UP);
  int dd = get_center_distance (me,DOWN);

  if (sign (dd - du))
    return Direction (sign (dd -du));

  return to_dir (me->get_grob_property ("neutral-direction"));
}

Real
Stem::get_default_stem_end_position (Grob*me) 
{
  bool grace_b = to_boolean (me->get_grob_property ("grace"));
  SCM s;
  Array<Real> a;

  Real length_f = 0.;
  SCM scm_len = me->get_grob_property ("length");
  if (gh_number_p (scm_len))
    {
      length_f = gh_scm2double (scm_len);
    }
  else
    {
      s = me->get_grob_property ("lengths");
      for (SCM q = s; q != SCM_EOL; q = gh_cdr (q))
	a.push (gh_scm2double (gh_car (q)));
		
      // stem uses half-spaces
      length_f = a[ ((flag_i (me) - 2) >? 0) <? (a.size () - 1)] * 2;
    }


  a.clear ();
  s = me->get_grob_property ("stem-shorten");
  for (SCM q = s; gh_pair_p (q); q = gh_cdr (q))
    a.push (gh_scm2double (gh_car (q)));


  // stem uses half-spaces

  // fixme: use gh_list_ref () iso. array[]
  Real shorten_f = a[ ((flag_i (me) - 2) >? 0) <? (a.size () - 1)] * 2;

  /* URGURGURG
     'set-default-stemlen' sets direction too
   */
  Direction dir = get_direction (me);
  if (!dir)
    {
      dir = get_default_dir (me);
      Directional_element_interface::set (me, dir);
    }
  
  /* 
    stems in unnatural (forced) direction should be shortened, 
    according to [Roush & Gourlay]
   */
  if (( (int)chord_start_f (me))
      && (get_direction (me) != get_default_dir (me)))
    length_f -= shorten_f;

  Interval hp = head_positions (me);  
  Real st = hp[dir] + dir * length_f;


  

  /*
    Make a little room if we have a flag and there is a dot.

    TODO:

    maybe  we should consider moving the dot to the right?
  */
  if (!beam_l (me)
      && flag_i (me))
    {
      Grob * closest_to_flag = extremal_heads (me)[dir];
      Grob * dots = closest_to_flag
	? Rhythmic_head::dots_l (closest_to_flag ) : 0;

      if (dots)
	{
	  Real dp = Staff_symbol_referencer::position_f  (dots);
	  Real flagy =  flag (me).extent (Y_AXIS)[-dir] * 2; // should divide by staffspace

	  /*
	    Very gory: add myself to the X-support of the parent,
	    which should be a dot-column.
	   */
	  if (dir * (st + flagy -  dp) < 0.5)
	    Side_position_interface::add_support (dots->parent_l (X_AXIS), me);

	  /*
	    previous approach was to lengthen the stem. This is not
	    good typesetting practice.  */
	}
    }


  bool no_extend_b = to_boolean (me->get_grob_property ("no-stem-extend"));
   if (!grace_b && !no_extend_b && dir * st < 0) // junkme?
      st = 0.0;

  return st;
}



/*
  Number of hooks on the flag, ie. the log of the duration.
 */
int
Stem::flag_i (Grob*me) 
{
  SCM s = me->get_grob_property ("duration-log");
  return (gh_number_p (s)) ? gh_scm2int (s) : 2;
}

void
Stem::position_noteheads (Grob*me)
{
  if (!heads_i (me))
    return;
  
  Link_array<Grob> heads =
    Pointer_group_interface__extract_elements (me, (Grob*)0, "heads");

  heads.sort (compare_position);
  Direction dir =get_direction (me);
  
  if (dir < 0)
    heads.reverse ();


  Grob *hed = support_head (me);
  Real w = hed->extent (hed, X_AXIS)[dir];
  for (int i=0; i < heads.size (); i++)
    {
      heads[i]->translate_axis (w - heads[i]->extent (heads[i], X_AXIS)[dir], X_AXIS);
    }
  
  bool parity= true;		// todo: make me settable.
  int lastpos = int (Staff_symbol_referencer::position_f (heads[0]));
  for (int i=1; i < heads.size (); i ++)
    {
      Real p = Staff_symbol_referencer::position_f (heads[i]);
      int dy =abs (lastpos- (int)p);

      if (dy <= 1)
	{
	  if (parity)
	    {
	      Real l = heads[i]->extent (heads[i], X_AXIS).length ();
	      heads[i]->translate_axis (l * get_direction (me), X_AXIS);
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
  stem_end_position (me);	// ugh. Trigger direction calc.
  position_noteheads (me);

  if (invisible_b (me))
    {
      me->remove_grob_property ("molecule-callback");
      // suicide ();
    }
  
  set_spacing_hints (me);
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
  return ly_interval2scm (iv);
}


/**
   set stem directions for hinting the optical spacing correction.

   Modifies DIR_LIST property of the Stem's Paper_column

   TODO: more advanced: supply height of noteheads as well, for more advanced spacing possibilities
 */
void
Stem::set_spacing_hints (Grob*me) 
{
  if (!invisible_b (me))
    {
      SCM scmdir  = gh_int2scm (get_direction (me));

      Item* item = dynamic_cast<Item*> (me);
      Item * col =  item->column_l ();
      SCM dirlist =col->get_grob_property ("dir-list");
      if (scm_c_memq (scmdir, dirlist) == SCM_BOOL_F)
	{
	  dirlist = gh_cons (scmdir, dirlist);
	  col->set_grob_property ("dir-list", dirlist);
	}
    }
}

Molecule
Stem::flag (Grob*me)
{
  /* TODO: rename flag-style into something more appropriate,
   e.g. "stroke-style", maybe with values "" (i.e. no stroke),
   "single" and "double".  Needs more discussion.
  */
  String style, fstyle, staffline_offs;
  SCM fst = me->get_grob_property ("flag-style");
  if (gh_string_p (fst))
    {
      fstyle = ly_scm2string (fst);
    }

  SCM st = me->get_grob_property ("style");
  if (gh_symbol_p (st))
    {
      style = (ly_scm2string (scm_symbol_to_string (st)));
    }
  else
    {
      style = "";
    }
  if (String::compare_i (style, "mensural") == 0)
    /* Mensural notation: For notes on staff lines, use different
       flags than for notes between staff lines.  The idea is that
       flags are always vertically aligned with the staff lines,
       regardless if the note head is on a staff line or between two
       staff lines.  In other words, the inner end of a flag always
       touches a staff line.
    */
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
	 note heads to a single stem in mensural notation).

      */
	Grob *first = first_head(me);
	int sz = Staff_symbol_referencer::line_count (me)-1;
	int p = (int)rint (Staff_symbol_referencer::position_f (first));
	staffline_offs = (((p ^ sz) & 0x1) == 0) ? "1" : "0";
    }
  else
    {
	staffline_offs = "";
    }
  char c = (get_direction (me) == UP) ? 'u' : 'd';
  String index_str
    = String ("flags-") + style + to_str (c) + staffline_offs + to_str (flag_i (me));
  Molecule m
    = Font_interface::get_default_font (me)->find_by_name (index_str);
  if (!fstyle.empty_b ())
    m.add_molecule (Font_interface::get_default_font (me)->find_by_name (String ("flags-") + to_str (c) + fstyle));
  return m;
}

MAKE_SCHEME_CALLBACK (Stem,dim_callback,2);
SCM
Stem::dim_callback (SCM e, SCM ax)
{
  Axis a = (Axis) gh_scm2int (ax);
  assert (a == X_AXIS);
  Grob *se = unsmob_grob (e);
  Interval r (0, 0);
  if (unsmob_grob (se->get_grob_property ("beam")) || abs (flag_i (se)) <= 2)
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
  
  
  Real y1 = Staff_symbol_referencer::position_f (first_head (me));
  Real y2 = stem_end_position (me);
  
  Interval stem_y (y1,y2);
  stem_y.unite (Interval (y2,y1));

  // dy?
  Real dy = Staff_symbol_referencer::staff_space (me)/2.0;
    
  if (Grob *hed = support_head (me))
    {
      Interval head_height = hed->extent (hed,Y_AXIS);
      Real y_attach = Note_head::stem_attachment_coordinate ( hed, Y_AXIS);

      y_attach = head_height.linear_combination (y_attach);
      stem_y[Direction (-d)] += d * 2*y_attach;
    }
  
  if (!invisible_b (me))
    {
      Real stem_width = gh_scm2double (me->get_grob_property ("thickness"))
	// URG
	* me->paper_l ()->get_var ("stafflinethickness");
      
      Molecule ss =Lookup::filledbox (Box (Interval (-stem_width/2, stem_width/2),
					   Interval (stem_y[DOWN]*dy, stem_y[UP]*dy)));
      mol.add_molecule (ss);
    }

  if (!beam_l (me) && abs (flag_i (me)) > 2)
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
  if (Grob * f = first_head (me))
    {
      Interval head_wid = f->extent (f,X_AXIS);

      Real attach =
	Note_head::stem_attachment_coordinate(f, X_AXIS);

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
	    * me->paper_l ()->get_var ("stafflinethickness");

	  
	  r += - d * rule_thick * 0.5;
	}
    }
  return gh_double2scm (r);
}



Grob*
Stem::beam_l (Grob*me)
{
  SCM b=  me->get_grob_property ("beam");
  return unsmob_grob (b);
}


// ugh still very long.
Stem_info
Stem::calc_stem_info (Grob*me) 
{
  Grob * beam = beam_l (me);

  Direction beam_dir = Directional_element_interface::get (beam);
  if (!beam_dir)
    {
      programming_error ("Beam dir not set.");
      beam_dir = UP;
    }
    

  Real staff_space = Staff_symbol_referencer::staff_space (me);
  Real half_space = staff_space / 2;
  int multiplicity = Beam::get_multiplicity (beam);


  SCM space_proc = beam->get_grob_property ("space-function");
  SCM space = gh_call1 (space_proc, gh_int2scm (multiplicity));
  Real interbeam_f = gh_scm2double (space) * staff_space;

  Real thick = gh_scm2double (beam->get_grob_property ("thickness"));
  Stem_info info; 
  info.idealy_f_ = chord_start_f (me);

  // for simplicity, we calculate as if dir == UP
  info.idealy_f_ *= beam_dir;
  SCM grace_prop = me->get_grob_property ("grace");

  bool grace_b = to_boolean (grace_prop);
  
  Array<Real> a;
  SCM s;
  
  s = me->get_grob_property ("beamed-minimum-lengths");
  a.clear ();
  for (SCM q = s; q != SCM_EOL; q = gh_cdr (q))
    a.push (gh_scm2double (gh_car (q)));


  Real minimum_length = a[multiplicity <? (a.size () - 1)] * staff_space;
  s = me->get_grob_property ("beamed-lengths");

  a.clear ();
  for (SCM q = s; q != SCM_EOL; q = gh_cdr (q))
    a.push (gh_scm2double (gh_car (q)));

  Real stem_length =  a[multiplicity <? (a.size () - 1)] * staff_space;

  if (!beam_dir || (beam_dir == Directional_element_interface::get (me)))
    /* normal beamed stem */
    {
      if (multiplicity)
	{
	  info.idealy_f_ += thick + (multiplicity - 1) * interbeam_f;
	}
      info.miny_f_ = info.idealy_f_;
      info.maxy_f_ = INT_MAX;

      info.idealy_f_ += stem_length;
      info.miny_f_ += minimum_length;

      /*
	lowest beam of (UP) beam must never be lower than second staffline

	Hmm, reference (Wanske?)

	Although this (additional) rule is probably correct,
	I expect that highest beam (UP) should also never be lower
	than middle staffline, just as normal stems.
	
      */
      bool no_extend_b = to_boolean (me->get_grob_property ("no-stem-extend"));
      if (!grace_b && !no_extend_b)
	{
	  /* highest beam of (UP) beam must never be lower than middle
	     staffline
	     lowest beam of (UP) beam must never be lower than second staffline
	   */
	  info.miny_f_ =
	    info.miny_f_ >? 0
	    >? (- 2 * half_space - thick
		+ (multiplicity > 0) * thick
		+ interbeam_f * (multiplicity - 1));
	}
    }
  else
    /* knee */
    {
      info.idealy_f_ -= thick;
      info.maxy_f_ = info.idealy_f_;
      info.miny_f_ = -INT_MAX;

      info.idealy_f_ -= stem_length;
      info.maxy_f_ -= minimum_length;
    }
  
  info.idealy_f_ = (info.maxy_f_ <? info.idealy_f_) >? info.miny_f_;

  s = beam->get_grob_property ("shorten");
  if (gh_number_p (s))
    info.idealy_f_ -= gh_scm2double (s);

 Grob *common = me->common_refpoint (beam, Y_AXIS);
  Real interstaff_f = beam_dir *
 (me->relative_coordinate (common, Y_AXIS)
     - beam->relative_coordinate (common, Y_AXIS));

  info.idealy_f_ += interstaff_f;
  info.miny_f_ += interstaff_f;
  info.maxy_f_ += interstaff_f ;

  return info;
}

bool
Stem::has_interface (Grob*m)
{
  return m && m->has_interface (ly_symbol2scm ("stem-interface"));
}

void
Stem::set_interface (Grob*me)
{    
  me->set_interface (ly_symbol2scm ("stem-interface"));
}
