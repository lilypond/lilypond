/*
  new-system-start-delimiter-engraver.cc -- implement
  Nested_system_start_delimiter_engraver

  source file of the GNU LilyPond music typesetter

  (c) 2005 Han-Wen Nienhuys <hanwen@xs4all.nl>

*/

#include "side-position-interface.hh"
#include "system-start-delimiter.hh"
#include "engraver.hh"
#include "staff-symbol.hh"
#include "pointer-group-interface.hh"
#include "paper-column.hh"
#include "output-def.hh"
#include "spanner.hh"


struct Bracket_nesting_node
{
public:
  virtual ~Bracket_nesting_node(){}
  virtual bool add_staff (Grob *) { return false; }
  virtual void add_support (Grob *) { }
  virtual void set_bound (Direction, Grob *){}
  virtual void set_nesting_support (Grob*) {}
  virtual void create_grobs (Engraver*, SCM) {}
};

struct Bracket_nesting_group : public Bracket_nesting_node
{
  Spanner *delimiter_;
  Link_array<Bracket_nesting_node> children_;
  SCM symbol_;

  void from_list (SCM ); 
  virtual void add_support (Grob *grob);
  virtual bool add_staff (Grob *grob);
  virtual void set_nesting_support (Grob*);
  virtual void set_bound (Direction, Grob *grob);
  virtual void create_grobs (Engraver*, SCM);
  ~Bracket_nesting_group ();
  Bracket_nesting_group ();
};


struct Bracket_nesting_staff : public Bracket_nesting_node
{
  Grob *staff_;

  Bracket_nesting_staff (Grob *s) { staff_ = s; }
  virtual bool add_staff (Grob *);
};


Bracket_nesting_group::Bracket_nesting_group ()
{
  symbol_ = SCM_EOL;
  delimiter_ = 0;
}

bool
Bracket_nesting_staff::add_staff (Grob *g)
{
  if (!staff_)
    {
      staff_ = g;
      return true;
    }
  return false;
}

void
Bracket_nesting_group::create_grobs (Engraver *engraver, SCM default_type)
{
  SCM type = scm_is_symbol (symbol_) ? symbol_ : default_type;
  delimiter_ = make_spanner_from_properties (engraver, type,
					     SCM_EOL, ly_symbol2string (type).to_str0 ());

  for (int i = 0 ; i < children_.size (); i++)
    {
      children_[i]->create_grobs (engraver, default_type);
    }
}

void
Bracket_nesting_group::add_support (Grob *g)
{
  Side_position_interface::add_support (g, delimiter_);
  for (int i = 0 ; i < children_.size (); i++)
    {
      children_[i]->add_support (g);
    }
}

Bracket_nesting_group::~Bracket_nesting_group ()
{
  for (int i = 0 ; i < children_.size (); i++)
    delete children_[i];
}

void
Bracket_nesting_group::set_bound (Direction d, Grob *g)
{
  delimiter_->set_bound (d, g);
  for (int i = 0 ; i < children_.size (); i++)
    {
      children_[i]->set_bound (d, g);
    }
}

void
Bracket_nesting_group::set_nesting_support (Grob *parent)
{
  if (parent)
    Side_position_interface::add_support (delimiter_, parent);
  
  for (int i = 0 ; i < children_.size (); i++)
    {
      children_[i]->set_nesting_support (delimiter_);
    }
}


void
Bracket_nesting_group::from_list (SCM x)
{
  for (SCM s = x; scm_is_pair (s); s = scm_cdr (s))
    {
      SCM entry = scm_car (s);
      if (scm_is_pair (entry))
	{
	  Bracket_nesting_group *node = new Bracket_nesting_group;
	  node->from_list (entry);
	  children_.push (node);
	}
      else
	{
	  children_.push (new Bracket_nesting_staff (0));
	}

      if (scm_is_symbol (entry))
	symbol_ = entry;
    }
}

bool
Bracket_nesting_group::add_staff (Grob *grob)
{
  for (int i = 0; i < children_.size (); i++)
    {
      if (children_[i]->add_staff (grob))
	{
	  Pointer_group_interface::add_grob (delimiter_, ly_symbol2scm ("elements"), grob);
	  return true;
	}
    }

  return false;
}




/****************/

class Nested_system_start_delimiter_engraver : public Engraver
{
public:
  TRANSLATOR_DECLARATIONS (Nested_system_start_delimiter_engraver);

protected:
  Bracket_nesting_group *nesting_;
  
  DECLARE_ACKNOWLEDGER (system_start_delimiter);
  DECLARE_ACKNOWLEDGER (staff_symbol);

  void process_music ();
  virtual void finalize ();
};

Nested_system_start_delimiter_engraver::Nested_system_start_delimiter_engraver ()
{
  nesting_ = 0;
}



void
Nested_system_start_delimiter_engraver::process_music ()
{
  if (!nesting_)
    {
      nesting_ = new Bracket_nesting_group ();
      SCM hierarchy = get_property ("systemStartDelimiterHierarchy");
      SCM delimiter_name = get_property ("systemStartDelimiter");

      nesting_->from_list (hierarchy);
      nesting_->create_grobs (this, delimiter_name);
      nesting_->set_bound (LEFT, unsmob_grob (get_property ("currentCommandColumn")));
    }
}

void
Nested_system_start_delimiter_engraver::finalize ()
{
  if (nesting_)
    {
      nesting_->set_bound (RIGHT,
			   unsmob_grob (get_property ("currentCommandColumn")));
      nesting_->set_nesting_support (0);
    }
}

void
Nested_system_start_delimiter_engraver::acknowledge_staff_symbol (Grob_info inf)
{
  Grob *staff = inf.grob();
  bool succ = nesting_->add_staff (staff);

  if (!succ)
    {
      nesting_->children_.push  (new Bracket_nesting_staff (0));
      nesting_->add_staff (staff);
    }
}


void
Nested_system_start_delimiter_engraver::acknowledge_system_start_delimiter (Grob_info inf)
{
  nesting_->add_support (inf.grob ());
}

#include "translator.icc"

ADD_ACKNOWLEDGER (Nested_system_start_delimiter_engraver, staff_symbol);
ADD_ACKNOWLEDGER (Nested_system_start_delimiter_engraver, system_start_delimiter);

ADD_TRANSLATOR (Nested_system_start_delimiter_engraver,
		/* doc */ "Creates a system start delimiter (ie. SystemStart@{Bar, Brace, Bracket@} spanner",
		/* create */ "SystemStartSquare SystemStartBrace SystemStartBracket SystemStartBar",
		/* accept */ "",
		/* read */ "systemStartDelimiter systemStartDelimiterHierarchy currentCommandColumn",
		/* write */ "");
