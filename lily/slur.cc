/*
  slur.cc -- implement  Slur

  source file of the GNU LilyPond music typesetter

  (c) 1996, 1997 Han-Wen Nienhuys <hanwen@stack.nl>
*/

/*
  TODO:
  
  think about crossing stems.
  Begin and end should be treated as a Script.
 */
#include "slur.hh"
#include "scalar.hh"
#include "lookup.hh"
#include "paper-def.hh"
#include "note-column.hh"
#include "stem.hh"
#include "p-col.hh"
#include "molecule.hh"
#include "debug.hh"
#include "boxes.hh"
#include "bezier.hh"
#include "main.hh"

IMPLEMENT_IS_TYPE_B1(Slur,Spanner);

void
Slur::add (Note_column*n)
{
  encompass_arr_.push (n);
  add_dependency (n);
}

void
Slur::set_default_dir ()
{
  dir_ = DOWN;
  for (int i=0; i < encompass_arr_.size (); i ++) 
    {
      if (encompass_arr_[i]->dir_ < 0) 
	{
	  dir_ = UP;
	  break;
	}
    }
}

void
Slur::do_add_processing ()
{
  set_bounds (LEFT, encompass_arr_[0]);    
  if (encompass_arr_.size () > 1)
    set_bounds (RIGHT, encompass_arr_.top ());
}

void
Slur::do_pre_processing ()
{
  // don't set directions
}

void
Slur::do_substitute_dependency (Score_elem*o, Score_elem*n)
{
  int i;
  while ((i = encompass_arr_.find_i ((Note_column*)o->item ())) >=0) 
    {
      if (n)
	encompass_arr_[i] = (Note_column*)n->item ();
      else
	encompass_arr_.del (i);
    }
}

static int 
Note_column_compare (Note_column *const&n1 , Note_column* const&n2)
{
  return Item::left_right_compare (n1, n2);
}

void
Slur::do_post_processing ()
{
  encompass_arr_.sort (Note_column_compare);
  if (!dir_)
    set_default_dir ();
  Real interline_f = paper ()->interline_f ();
  Real inter_f = interline_f / 2;

  /* 
   [OSU]: slur and tie placement

   slurs:
   * x = centre of head (upside-down: inner raakpunt stem) - d * gap

   * y = length < 5ss : horizontal raakpunt + d * 0.25 ss
     y = length >= 5ss : y next interline - d * 0.25 ss
     --> height <= 5 length ?? we use <= 3 length, now...

   * suggested gap = ss / 5;
   */
  // jcn: 1/5 seems so small?
  Real gap_f = interline_f / 2; // 5;
  
  Drul_array<Note_column*> extrema;
  extrema[LEFT] = encompass_arr_[0];
  extrema[RIGHT] = encompass_arr_.top ();

  Direction d=LEFT;
  Real nw_f = paper ()->note_width ();
 
  do 
    {
      if (extrema[d] != spanned_drul_[d]) 
	{
	  dx_f_drul_[d] = -d 
	    *(spanned_drul_[d]->width ().length () -0.5*nw_f);
	}
      else if (extrema[d]->stem_l_ && !extrema[d]->stem_l_->transparent_b_) 
        {
	  dy_f_drul_[d] = (int)rint (extrema[d]->stem_l_->height ()[dir_]);
	  /* normal slur from notehead centre to notehead centre, minus gap */
	  // ugh: diff between old and new slurs
	  if (!experimental_features_global_b)
	    dx_f_drul_[d] += -d * gap_f;
	  else
	    dx_f_drul_[d] += 0.5 * nw_f - d * gap_f;
	}
      else 
        {
	  dy_f_drul_[d] = (int)rint (extrema[d]->head_positions_interval ()[dir_])* inter_f;
	}
      dy_f_drul_[d] += dir_ * interline_f;
    }
  while (flip(&d) != LEFT);
}

Real
Slur::height_f () const
{
  Bezier_bow bow (paper ());
  Array<Offset> notes = get_notes ();
  bow.set (notes, dir_);

  Real height = 0;
  Real dy1 = bow.calc_f (height);
  if (!dy1)
    return height;

  height = dy1;
  bow.set (notes, dir_);
  Real dy2 = bow.calc_f (height);
  if (!dy2)
    return height;

  if (abs (dy2 - dy1) < paper ()->rule_thickness ())
    return height;
  
  /*
    Assume 
      dy = B (h) 
    with 
      B (h) = a * h + b;

    Then we get for height = h{dy=0}
   */
  Real a = (dy2 - dy1) / dy1;
  Real b = dy1;
  height = -b / a;
    
  if (check_debug && !monitor->silent_b ("Slur")) 
    { 
      cout << "************" << endl;
      cout << "dy1: " << dy1 << endl;
      cout << "dy2: " << dy2 << endl;
      cout << "a: " << a << endl;
      cout << "b: " << b << endl;
      cout << "h: " << height << endl;
    }

  return height;
}

Molecule*
Slur::brew_molecule_p () const
{
  if (!experimental_features_global_b)
    return Bow::brew_molecule_p ();

  Molecule* mol_p = new Molecule;
  
  Real dy_f = dy_f_drul_[RIGHT] - dy_f_drul_[LEFT];
  
  Real dx_f = width ().length ();
  dx_f += (dx_f_drul_[RIGHT] - dx_f_drul_[LEFT]);
  
  Atom a = paper ()->lookup_l ()->control_slur (get_controls (), dx_f, dy_f);

  Real interline_f = paper ()->interline_f ();
  Real gap_f = interline_f / 2; // 5;
  Real nw_f = paper ()->note_width ();
  a.translate (Offset (dx_f + 0.5 * nw_f + gap_f, dy_f + dy_f_drul_[LEFT]));
  mol_p->add (a);
  return mol_p;
}

Array<Offset>
Slur::get_notes () const
{
  Real interline = paper ()->interline_f ();
  Real notewidth = paper ()->note_width ();
  Real internote = interline / 2;

  Stem* left_stem = encompass_arr_[0]->stem_l_;
  Real left_x = left_stem->hpos_f ();
  // ugh, do bow corrections (see brew_mol)
  left_x += dx_f_drul_[LEFT] + 0.5 * notewidth;

  // ugh, do bow corrections (see brew_mol)
  Real left_y = dy_f_drul_[LEFT];
  // ugh, where does this asymmetry come from?
  if (dir_ == DOWN)
    left_y -= dir_ * internote;

  /*
    urg, corrections for broken slurs: extra begin or end position 
   */
  int first = 0;
  int n = encompass_arr_.size ();
  if (encompass_arr_[0] != spanned_drul_[LEFT])
    {
      n += 1;
      first = 1;
      left_x = spanned_drul_[LEFT]->width ().length ();
      left_y = 0;
    }
  if (encompass_arr_.top () != spanned_drul_[RIGHT])
      n += 1;

  Array<Offset> notes;
  notes.set_size (n);

  Real dx = width ().length ();
  dx += (dx_f_drul_[RIGHT] - dx_f_drul_[LEFT]);
  dx = dx <? 1000;
  dx = dx >? 2 * interline;
    
  Real dy = (dy_f_drul_[RIGHT] - dy_f_drul_[LEFT]);
  if (abs (dy) > 1000)
    dy = sign (dy) * 1000;

  notes[0].x () = 0;
  notes[0].y () = 0;
  notes[n - 1].x () = dx;
  notes[n - 1].y () = dy;
  for (int i = 1; i < n - 1; i++)
    {
      Stem* stem = encompass_arr_[i - first]->stem_l_;
      /* 
	set x to middle of notehead or on exact x position of stem,
	according to slur direction
	   */
      Real x = stem->hpos_f () - left_x + notewidth / 2;
      if (stem->dir_ != dir_)
	x += notewidth / 2;
      else if (stem->dir_ == UP)
	x += notewidth;
      Real y = stem->dir_ == dir_ ? stem->stem_end_f ()
	: stem->stem_begin_f () + 2.5 * dir_;

      /*
	leave a gap: slur mustn't touch head/stem
       */
      y += 2.5 * dir_;
      y *= internote;
      y -= left_y;

      notes[i].x () = x;
      notes[i].y () = y;
    }
  return notes;
}

Array<Offset>
Slur::get_controls () const
{
  Bezier_bow b (paper ());
  b.set (get_notes (), dir_);
  b.calc ();
  Array<Offset> controls;
  controls.set_size (8);
  for (int i = 0; i < 4; i++)
    controls[i] = b.control_[i];
  for (int i = 0; i < 4; i++)
    controls[i + 4] = b.return_[i];
  return controls;
}

