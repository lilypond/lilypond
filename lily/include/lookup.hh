/*
  lookup.hh -- declare Lookup

  source file of the GNU LilyPond music typesetter

  (c) 1997--1998 Han-Wen Nienhuys <hanwen@cs.uu.nl>
  Jan Nieuwenhuizen <janneke@gnu.org>
*/

#ifndef LOOKUP_HH
#define LOOKUP_HH

#include "atom.hh"
#include "fproto.hh"
#include "scalar.hh"
#include "direction.hh"
#include "curve.hh"
#include "afm.hh"
#include "symtable.hh"
#include "box.hh"

/** handy interface to symbol table
 */
class Lookup
{
public:
  Lookup ();
  Lookup (Lookup const&);
  Lookup (Symtables const&);
  virtual ~Lookup ();
  
  Atom accidental (int) const;
  void add (String, Symtable*);
  virtual Atom afm_find (String s) const = 0;
  Atom afm_find (String, String) const;
  virtual Atom* atom_p (String, int, Box) const = 0;
  Atom ball (int) const;
  Atom bar (String, Real height) const;
  String base_output_str () const;
  Atom beam (Real,Real, Real) const;
  virtual String character_str (int i) const;
  Atom clef (String) const;
  virtual Atom dashed_slur (Array<Offset> controls, Real thick, Real dash) const = 0;
  Atom dots () const;
  Atom dynamic (String) const;
  Atom fill (Box b) const;
  Atom flag (int, Direction) const;
  virtual Atom hairpin (Real width, bool decresc, bool continued) const = 0;
  virtual Lookup* lookup_p (Lookup const&) const = 0;
  virtual Lookup* lookup_p (Symtables const&) const = 0;
  virtual Paper_outputter* paper_outputter_p (Paper_stream*, Paper_def*, Scope*, String) const = 0;
  virtual Paper_stream* paper_stream_p () const = 0;
  virtual Atom plet (Real dy, Real dx, Direction dir) const = 0;
  void print () const;
  virtual Atom ps_beam (Real slope, Real width, Real thick) const = 0;
  virtual String print_dimen (Real) const;
  Atom rest (int, bool outside) const;
  Atom rule_symbol (Real height, Real width) const;
  Atom script (String idx) const;
  /** paratime_signature substitution in lookup strings.
      this function provides a simple macro mechanism:

      if source == "tex%bla%", then
      substitute_args (source, {"X","Y"})  == "texXblaY"
  */
  String substitute_args (String source, Array<String> args) const;
  /// paratime_signature substitution in lookup strings
  String substitute_args (String source, Array<Scalar> args) const;
  virtual Atom stem (Real y1_pos, Real y2_pos) const = 0;
  Atom stem (Real y1_pos, Real y2_pos, String) const;
  virtual Atom slur (Array<Offset> controls) const = 0;
  Atom streepje (int type) const;
  virtual Atom text (String style, String text) const;
  virtual String unknown_str () const = 0;
  Atom vbrace (Real &dy) const;
  virtual Atom vbracket (Real &dy) const = 0;
  Atom special_time_signature (String, Array<Scalar>) const;
  Atom time_signature (Array<Scalar>) const;

  Paper_def * paper_l_;
  Symtables *symtables_p_;
  String font_;
  String font_path_;  
  Adobe_font_metric * afm_p_;
};

#endif // LOOKUP_HH
