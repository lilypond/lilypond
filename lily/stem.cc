/*
  stem.cc -- implement Stem

  source file of the GNU LilyPond music typesetter

  (c) 1996, 1997--1999 Han-Wen Nienhuys <hanwen@cs.uu.nl>

  TODO: This is way too hairy
*/
#include "dimension-cache.hh"
#include "stem.hh"
#include "debug.hh"
#include "paper-def.hh"
#include "note-head.hh"
#include "lookup.hh"
#include "molecule.hh"
#include "paper-column.hh"
#include "misc.hh"
#include "beam.hh"
#include "rest.hh"
#include "group-interface.hh"

Stem::Stem ()
{
  beams_i_drul_[LEFT] = beams_i_drul_[RIGHT] = -1;
  yextent_drul_[DOWN] = yextent_drul_[UP] = 0;
  flag_i_ = 2;
}

Interval_t<int>
Stem::head_positions () const
{
  /* 
    Mysterious FreeBSD fix by John Galbraith.  Somehow, the empty intervals 
    trigger FP exceptions on FreeBSD.  Fix: do not return infinity 

   */
  if (!first_head ())
    {
      return Interval_t<int> (100,-100);	
    }

  Link_array<Note_head> head_l_arr =
    Group_interface__extract_elements (this, (Note_head*)0, "heads");
  
  Interval_t<int> r;
  for (int i =0; i < head_l_arr.size (); i++)
    {
      int p = (int)head_l_arr[i]->position_f ();
      r[BIGGER] = r[BIGGER] >? p;
      r[SMALLER] = r[SMALLER] <? p;
    }
  return r;
}

void
Stem::do_print () const
{
#ifndef NPRINT
  DEBUG_OUT << "flag "<< flag_i_;
#endif
}

Real
Stem::stem_length_f () const
{
  return yextent_drul_[UP]-yextent_drul_[DOWN] ;
}

Real
Stem::stem_begin_f () const
{
  return yextent_drul_[Direction(-get_direction ())];
}

Real
Stem::chord_start_f () const
{
  return head_positions()[get_direction ()] * staff_line_leading_f ()/2.0;
}

Real
Stem::stem_end_f () const
{
  return yextent_drul_[get_direction ()];
}

void
Stem::set_stemend (Real se)
{
  // todo: margins
  if (get_direction () && get_direction () * head_positions()[get_direction ()] >= se*get_direction ())
    warning (_ ("Weird stem size; check for narrow beams"));

  
  yextent_drul_[get_direction ()]  =  se;
  yextent_drul_[Direction(-get_direction ())] = head_positions()[-get_direction ()];
}

int
Stem::type_i () const
{
  
  return first_head ()->balltype_i_;
}

Note_head*
Stem::first_head () const
{
  SCM h =get_elt_property ("heads");
  if (!gh_pair_p (h))
    return 0;

  Score_element * sc = unsmob_element (gh_car (h));

  return dynamic_cast<Note_head*> (sc);
}

void
Stem::add_head (Rhythmic_head *n)
{
  n->set_elt_property ("stem", this->self_scm_);
  n->add_dependency (this);	// ?
  

  Group_interface gi (this);
  if (Note_head *nh = dynamic_cast<Note_head *> (n))
    gi.name_ = "heads";
  else
    gi.name_ = "rests";

  gi.add_element (n);
}

bool
Stem::invisible_b () const
{
  return !(first_head () && first_head()->balltype_i_ >= 1);
}

int
Stem::get_center_distance (Direction d) const
{
  int staff_center = 0;
  int distance = d*(head_positions()[d] - staff_center);
  return distance >? 0;
}

Direction
Stem::get_default_dir () const
{
  int du = get_center_distance (UP);
  int dd = get_center_distance (DOWN);

  if (sign (dd - du))
    return Direction (sign (dd -du));

  return Direction (int(paper_l ()->get_var ("stem_default_neutral_direction")));
}



void
Stem::set_default_stemlen ()
{
  Real length_f = 0.;
  SCM scm_len = get_elt_property("length");
  if (scm_len != SCM_UNDEFINED)
    {
      length_f = gh_scm2double (scm_len);
    }
  else
    length_f = paper_l ()->get_var ("stem_length0");

  bool grace_b = get_elt_property ("grace") != SCM_UNDEFINED;
  String type_str = grace_b ? "grace_" : "";

  Real shorten_f = paper_l ()->get_var (type_str + "forced_stem_shorten0");

  if (!get_direction ())
    set_direction (get_default_dir ());

  /* 
    stems in unnatural (forced) direction should be shortened, 
    according to [Roush & Gourlay]
   */
  if (((int)chord_start_f ())
      && (get_direction () != get_default_dir ()))
    length_f -= shorten_f;

  if (flag_i_ >= 5)
    length_f += 2.0;
  if (flag_i_ >= 6)
    length_f += 1.0;
  
  set_stemend ((get_direction () > 0) ? head_positions()[BIGGER] + length_f:
	       head_positions()[SMALLER] - length_f);

  bool no_extend_b = get_elt_property ("no-stem-extend") != SCM_UNDEFINED;
  if (!grace_b && !no_extend_b && (get_direction () * stem_end_f () < 0))
    set_stemend (0);
}

//xxx
void
Stem::set_default_extents ()
{
  if (!stem_length_f ())
    set_default_stemlen ();

}

void
Stem::set_noteheads ()
{
  if (!first_head ())
    return;

  
  Link_array<Note_head> head_l_arr =
    Group_interface__extract_elements (this, (Note_head*)0, "heads");

  head_l_arr.sort (Note_head::compare);
  if (get_direction () < 0)
    head_l_arr.reverse ();

  Note_head * beginhead =   first_head ();
  beginhead->set_elt_property ("extremal", SCM_BOOL_T);
  if  (beginhead !=   head_l_arr.top ())
    head_l_arr.top ()->set_elt_property ("extremal", SCM_BOOL_T);
  
  int parity=1;
  int lastpos = int (beginhead->position_f ());
  for (int i=1; i < head_l_arr.size (); i ++)
    {
      int dy =abs (lastpos- (int)head_l_arr[i]->position_f ());

      if (dy <= 1)
	{
	  if (parity)
	    head_l_arr[i]->flip_around_stem (get_direction ());
	  parity = !parity;
	}
      else
	parity = 1;
      lastpos = int (head_l_arr[i]->position_f ());
    }
}

void
Stem::do_pre_processing ()
{
  if (yextent_drul_[DOWN]== yextent_drul_[UP])
    set_default_extents ();
  set_noteheads ();

  if (invisible_b ())
    {
      set_elt_property ("transparent", SCM_BOOL_T);
      set_empty (Y_AXIS);      
      set_empty (X_AXIS);      
    }


  set_spacing_hints ();
}



/**
   set stem directions for hinting the optical spacing correction.

   Modifies DIR_LIST property of the Stem's Score_column

   TODO: more advanced: supply height of noteheads as well, for more advanced spacing possibilities
 */

void
Stem::set_spacing_hints () 
{
  if (!invisible_b ())
    {
      SCM scmdir  = gh_int2scm (get_direction ());
      SCM dirlist = column_l ()->get_elt_property ("dir-list");
      if (dirlist == SCM_UNDEFINED)
	dirlist = SCM_EOL;

      if (scm_sloppy_memq (scmdir, dirlist) == SCM_EOL)
	{
	  dirlist = gh_cons (scmdir, dirlist);
	  column_l ()->set_elt_property ("dir-list", dirlist);
	}
    }
}

Molecule
Stem::flag () const
{
  String style;
  SCM st = get_elt_property ("style");
  if ( st != SCM_UNDEFINED)
    {
      style = ly_scm2string (st);
    }

  char c = (get_direction () == UP) ? 'u' : 'd';
  Molecule m = lookup_l ()->afm_find (String ("flags-") + to_str (c) + 
				      to_str (flag_i_));
  if (!style.empty_b ())
    m.add_molecule(lookup_l ()->afm_find (String ("flags-") + to_str (c) + style));
  return m;
}

Interval
Stem::dim_callback (Dimension_cache const* c) 
{
  Stem * s = dynamic_cast<Stem*> (c->element_l ());
  
  Interval r (0, 0);
  if (s->get_elt_property ("beam") != SCM_UNDEFINED || abs (s->flag_i_) <= 2)
    ;	// TODO!
  else
    {
      r = s->flag ().dim_.x ();
      r += s->note_delta_f ();
    }
  return r;
}




const Real ANGLE = 20* (2.0*M_PI/360.0); // ugh!

Molecule*
Stem::do_brew_molecule_p () const
{
  Molecule *mol_p =new Molecule;
  Drul_array<Real> stem_y = yextent_drul_;
  Real dy = staff_line_leading_f ()/2.0;

  Real head_wid = 0;
  if (first_head ())
    head_wid = first_head ()->extent (X_AXIS).length ();
  stem_y[Direction(-get_direction ())] += get_direction () * head_wid * tan(ANGLE)/(2*dy);
  
  if (!invisible_b ())
    {
      Real stem_width = paper_l ()->get_var ("stemthickness");
      Molecule ss =lookup_l ()->filledbox (Box (Interval (-stem_width/2, stem_width/2),
						 Interval (stem_y[DOWN]*dy, stem_y[UP]*dy)));
      mol_p->add_molecule (ss);
    }

  if (get_elt_property ("beam") == SCM_UNDEFINED
      && abs (flag_i_) > 2)
    {
      Molecule fl = flag ();
      fl.translate_axis(stem_y[get_direction ()]*dy, Y_AXIS);
      mol_p->add_molecule (fl);
    }

  if (first_head ())
    {
      mol_p->translate_axis (note_delta_f (), X_AXIS);
    }
  return mol_p;
}

Real
Stem::note_delta_f () const
{
  Real r=0;
  if (first_head ())
    {
      Interval head_wid(0,  first_head()->extent (X_AXIS).length ());
         Real rule_thick = paper_l ()->get_var ("stemthickness");

      Interval stem_wid(-rule_thick/2, rule_thick/2);
      if (get_direction () == CENTER)
	r = head_wid.center ();
      else
	r = head_wid[get_direction ()] - stem_wid[get_direction ()];
    }
  return r;
}

Real
Stem::hpos_f () const
{
  return note_delta_f () + Item::hpos_f ();
}


Beam*
Stem::beam_l ()const
{
  SCM b=  get_elt_property ("beam");
  return dynamic_cast<Beam*> (unsmob_element (b));
}
