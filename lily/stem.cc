/*
  stem.cc -- implement Stem

  source file of the GNU LilyPond music typesetter

  (c) 1996--2000 Han-Wen Nienhuys <hanwen@cs.uu.nl>
    Jan Nieuwenhuizen <janneke@gnu.org>

  TODO: This is way too hairy
*/
#include <math.h>		// m_pi

#include "directional-element-interface.hh"
#include "note-head.hh"
#include "stem.hh"
#include "debug.hh"
#include "paper-def.hh"
#include "rhythmic-head.hh"
#include "lookup.hh"
#include "molecule.hh"
#include "paper-column.hh"
#include "misc.hh"
#include "beam.hh"
#include "rest.hh"
#include "group-interface.hh"
#include "cross-staff.hh"
#include "staff-symbol-referencer.hh"
#include "spanner.hh"


void
Stem::set_beaming (Score_element*me ,int i,  Direction d )
{
  SCM pair = me->get_elt_property ("beaming");
  
  if (!gh_pair_p (pair))
    {
      pair = gh_cons (gh_int2scm (0),gh_int2scm (0));
      me->      set_elt_property ("beaming", pair);
    }
  index_set_cell (pair, d, gh_int2scm (i));
}

int
Stem::beam_count (Score_element*me,Direction d)
{
  SCM p=me->get_elt_property ("beaming");
  if (gh_pair_p (p))
    return gh_scm2int (index_cell (p,d));
  else
    return 0;
}

Interval
Stem::head_positions (Score_element*me) 
{
  if (!heads_i (me))
    {
      Interval iv;
      return iv;
    }

  Drul_array<Score_element*> e (extremal_heads (me));

  return Interval (Staff_symbol_referencer::position_f (e[DOWN]),
		   Staff_symbol_referencer::position_f ( e[UP]));
}


Real
Stem::chord_start_f (Score_element*me) 
{
  return head_positions(me)[get_direction (me)]
    * Staff_symbol_referencer::staff_space (me)/2.0;
}

Real
Stem::stem_end_position (Score_element*me) 
{
  SCM p =me->get_elt_property ("stem-end-position");
  Real pos;
  if (!gh_number_p (p))
    {

      pos = get_default_stem_end_position (me);
      me->set_elt_property ("stem-end-position", gh_double2scm (pos));
    }
  else
    pos = gh_scm2double (p);

  return pos;
}

Direction
Stem::get_direction (Score_element*me)
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
Stem::set_stemend (Score_element*me, Real se)
{
  // todo: margins
  Direction d= get_direction (me);
  
  if (d && d * head_positions(me)[get_direction (me)] >= se*d)
    warning (_ ("Weird stem size; check for narrow beams"));

  me->set_elt_property ("stem-end-position", gh_double2scm (se));
}

int
Stem::type_i (Score_element*me) 
{
  return first_head (me) ?  Rhythmic_head::balltype_i (first_head (me)) : 2;
}

/*
  Note head that determines hshift for upstems
 */ 
Score_element*
Stem::support_head (Score_element*me)
{
  SCM h = me->get_elt_property ("support-head");
  Score_element * nh = unsmob_element (h);
  if (nh)
    return nh;
  else if (heads_i (me) == 1)
    {
      /*
	UGH.
       */
      
      return unsmob_element (gh_car (me->get_elt_property ("heads")));
    }
  else
    return first_head (me);
}


int
Stem::heads_i (Score_element*me)
{
  return  Pointer_group_interface::count (me, "heads");
}

/*
  The note head which forms one end of the stem.  
 */
Score_element*
Stem::first_head (Score_element*me)
{
  return extremal_heads (me)[-get_direction (me)];
}

/*
  START is part where stem reaches `last' head. 
 */
Drul_array<Score_element*>
Stem::extremal_heads (Score_element*me) 
{
  const int inf = 1000000;
  Drul_array<int> extpos;
  extpos[DOWN] = inf;
  extpos[UP] = -inf;  
  
  Drul_array<Score_element *> exthead;
  exthead[LEFT] = exthead[RIGHT] =0;
  
  for (SCM s = me->get_elt_property ("heads"); gh_pair_p (s); s = gh_cdr (s))
    {
      Score_element * n = unsmob_element (gh_car (s));

      
      int p = int(Staff_symbol_referencer::position_f (n));

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

void
Stem::add_head (Score_element*me, Score_element *n)
{
  n->set_elt_property ("stem", me->self_scm ());
  n->add_dependency (me);

  if (Note_head::has_interface (n))
    {
      Pointer_group_interface::add_element (me, "heads",n);
    }
  else
    {
      n->set_elt_property ("rest", n->self_scm ());
    }
}

bool
Stem::invisible_b (Score_element*me)
{
  return !(heads_i (me) && Rhythmic_head::balltype_i (support_head (me)) >= 1);
}

int
Stem::get_center_distance (Score_element*me, Direction d)
{
  int staff_center = 0;
  int distance = (int) (d*(head_positions(me)[d] - staff_center));
  return distance >? 0;
}

Direction
Stem::get_default_dir (Score_element*me) 
{
  int du = get_center_distance (me,UP);
  int dd = get_center_distance (me,DOWN);

  if (sign (dd - du))
    return Direction (sign (dd -du));

  return to_dir (me->get_elt_property ("default-neutral-direction"));
}

Real
Stem::get_default_stem_end_position (Score_element*me) 
{
  bool grace_b = to_boolean (me->get_elt_property ("grace"));
  SCM s;
  Array<Real> a;

  Real length_f = 0.;
  SCM scm_len = me->get_elt_property("length");
  if (gh_number_p (scm_len))
    {
      length_f = gh_scm2double (scm_len);
    }
  else
    {
      s = me->get_elt_property("lengths");
      for (SCM q = s; q != SCM_EOL; q = gh_cdr (q))
	a.push (gh_scm2double (gh_car (q)));
		
      // stem uses half-spaces
      length_f = a[((flag_i (me) - 2) >? 0) <? (a.size () - 1)] * 2;
    }


  a.clear ();
  s = me->get_elt_property ("stem-shorten");
  for (SCM q = s; gh_pair_p (q); q = gh_cdr (q))
    a.push (gh_scm2double (gh_car (q)));


  // stem uses half-spaces

  // fixme: use gh_list_ref () iso. array[]
  Real shorten_f = a[((flag_i (me) - 2) >? 0) <? (a.size () - 1)] * 2;

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
  if (((int)chord_start_f (me))
      && (get_direction (me) != get_default_dir (me)))
    length_f -= shorten_f;


   Real st = head_positions(me)[dir] + dir * length_f;
  
   bool no_extend_b = to_boolean (me->get_elt_property ("no-stem-extend"));
   if (!grace_b && !no_extend_b && dir * st < 0) // junkme?
      st = 0.0;

  return st;
}

/*
  Number of hooks on the flag, ie. the log of the duration.
 */
int
Stem::flag_i (Score_element*me) 
{
  SCM s = me->get_elt_property ("duration-log");
  return  (gh_number_p (s)) ? gh_scm2int (s) : 2;
}

void
Stem::position_noteheads (Score_element*me)
{
  if (!heads_i (me))
    return;
  
  Link_array<Score_element> heads =
    Pointer_group_interface__extract_elements (me, (Score_element*)0, "heads");

  heads.sort (compare_position);
  Direction dir =get_direction (me);
  
  if (dir < 0)
    heads.reverse ();


  Score_element *hed = support_head (me);
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

MAKE_SCHEME_CALLBACK(Stem,before_line_breaking,1);
SCM
Stem::before_line_breaking (SCM smob)
{
  Score_element*me = unsmob_element (smob);
  stem_end_position (me);	// ugh. Trigger direction calc.
  position_noteheads (me);

  if (invisible_b (me))
    {
      me->remove_elt_property ("molecule-callback");
      // suicide();
    }
  
  set_spacing_hints (me);
  return SCM_UNSPECIFIED;
}



/**
   set stem directions for hinting the optical spacing correction.

   Modifies DIR_LIST property of the Stem's Paper_column

   TODO: more advanced: supply height of noteheads as well, for more advanced spacing possibilities
 */
void
Stem::set_spacing_hints (Score_element*me) 
{
  if (!invisible_b (me))
    {
      SCM scmdir  = gh_int2scm (get_direction (me));

      Item* item = dynamic_cast<Item*> (me);
      Item * col =  item->column_l ();
      SCM dirlist =col->get_elt_property ("dir-list");
      if (scm_sloppy_memq (scmdir, dirlist) == SCM_BOOL_F)
	{
	  dirlist = gh_cons (scmdir, dirlist);
	  col->set_elt_property ("dir-list", dirlist);
	}
    }
}

Molecule
Stem::flag (Score_element*me)
{
  String style;
  SCM st = me->get_elt_property ("flag-style");
  if ( gh_string_p (st))
    {
      style = ly_scm2string (st);
    }

  char c = (get_direction (me) == UP) ? 'u' : 'd';
  Molecule m = me->lookup_l ()->afm_find (String ("flags-") + to_str (c) + 
				      to_str (flag_i (me)));
  if (!style.empty_b ())
    m.add_molecule(me->lookup_l ()->afm_find (String ("flags-") + to_str (c) + style));
  return m;
}

MAKE_SCHEME_CALLBACK(Stem,dim_callback,2);
SCM
Stem::dim_callback (SCM e, SCM )
{
   Score_element *se = unsmob_element (e);
  Interval r (0, 0);
  if (unsmob_element (se->get_elt_property ("beam")) || abs (flag_i (se)) <= 2)
    ;	// TODO!
  else
    {
      r = flag (se).extent (X_AXIS);
    }
  return ly_interval2scm ( r);
}


const Real ANGLE = 20* (2.0*M_PI/360.0); // ugh! Should be settable.


MAKE_SCHEME_CALLBACK(Stem,brew_molecule,1);

SCM
Stem::brew_molecule (SCM smob) 
{
  Score_element*me = unsmob_element (smob);
  Molecule mol;
  Direction d = get_direction (me);
  
  
  Real y1 = Staff_symbol_referencer::position_f (first_head (me));
  Real y2 = stem_end_position (me);
  
  Interval stem_y(y1,y2);
  stem_y.unite (Interval (y2,y1));

  Real dy = Staff_symbol_referencer::staff_space (me)/2.0;
  Real head_wid = 0;
  
  if (Score_element *hed = support_head (me))
    head_wid = hed->extent (hed,X_AXIS).length ();
  stem_y[Direction(-d)] += d * head_wid * tan(ANGLE)/(2*dy);
  
  if (!invisible_b (me))
    {
      Real stem_width = gh_scm2double (me->get_elt_property ("thickness")) * me->paper_l ()->get_var ("stafflinethickness");
      Molecule ss =me->lookup_l ()->filledbox (Box (Interval (-stem_width/2, stem_width/2),
						 Interval (stem_y[DOWN]*dy, stem_y[UP]*dy)));
      mol.add_molecule (ss);
    }

  if (!beam_l (me) && abs (flag_i (me)) > 2)
    {
      Molecule fl = flag (me);
      fl.translate_axis(stem_y[d]*dy, Y_AXIS);
      mol.add_molecule (fl);
    }

  return mol.create_scheme();
}

MAKE_SCHEME_CALLBACK(Stem,off_callback,2);
SCM
Stem::off_callback (SCM element_smob, SCM )
{
  Score_element *me = unsmob_element (element_smob);
  
  Real r=0;
  if (Score_element * f = first_head (me))
    {
      Interval head_wid(0, f->extent (f,X_AXIS).length ());

      if (to_boolean (me->get_elt_property ("stem-centered")))
	return gh_double2scm ( head_wid.center ());
      
      Real rule_thick = gh_scm2double (me->get_elt_property ("thickness")) * me->paper_l ()->get_var ("stafflinethickness");
      Direction d = get_direction (me);
      r = head_wid[d] - d * rule_thick ;
    }
  return gh_double2scm (r);
}



Score_element*
Stem::beam_l (Score_element*me)
{
  SCM b=  me->get_elt_property ("beam");
  return unsmob_element (b);
}


// ugh still very long.
Stem_info
Stem::calc_stem_info (Score_element*me) 
{
  Score_element * beam = beam_l (me);

  Direction beam_dir = Directional_element_interface::get (beam);
  if (!beam_dir)
    {
      programming_error ("Beam dir not set.");
      beam_dir = UP;
    }
    

  Real staff_space = Staff_symbol_referencer::staff_space (me);
  Real half_space = staff_space / 2;
  int multiplicity = Beam::get_multiplicity (beam);


  SCM space_proc = beam->get_elt_property ("space-function");
  SCM space = gh_call1 (space_proc, gh_int2scm (multiplicity));
  Real interbeam_f = gh_scm2double (space) * staff_space;

  Real thick = gh_scm2double (beam->get_elt_property ("thickness"));
  Stem_info info; 
  info.idealy_f_ = chord_start_f (me);

  // for simplicity, we calculate as if dir == UP
  info.idealy_f_ *= beam_dir;
  SCM grace_prop = me->get_elt_property ("grace");

  bool grace_b = to_boolean (grace_prop);
  
  Array<Real> a;
  SCM s;
  
  s = me->get_elt_property("beamed-minimum-lengths");
  a.clear ();
  for (SCM q = s; q != SCM_EOL; q = gh_cdr (q))
    a.push (gh_scm2double (gh_car (q)));


  Real minimum_length = a[multiplicity <? (a.size () - 1)] * staff_space;
  s = me->get_elt_property ("beamed-lengths");

  a.clear();
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
      bool no_extend_b = to_boolean (me->get_elt_property ("no-stem-extend"));
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

  s = beam->get_elt_property ("shorten");
  if (gh_number_p (s))
    info.idealy_f_ -= gh_scm2double (s);

  Real interstaff_f = -beam_dir* calc_interstaff_dist (dynamic_cast<Item*> (me), dynamic_cast<Spanner*> (beam));

  info.idealy_f_ += interstaff_f;
  info.miny_f_ += interstaff_f;
  info.maxy_f_ += interstaff_f ;

  return info;
}

bool
Stem::has_interface (Score_element*m)
{
  return m && m->has_interface (ly_symbol2scm ("stem-interface"));
}

void
Stem::set_interface (Score_element*me)
{    
  me->set_interface (ly_symbol2scm ("stem-interface"));
}
