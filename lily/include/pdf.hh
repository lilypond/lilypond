/*
  pdf.hh -- declare Pdf output data structures.

  source file of the GNU LilyPond music typesetter

  (c) 2005 Han-Wen Nienhuys <hanwen@xs4all.nl>

*/

#ifndef PDF_HH
#define PDF_HH

#include <stdio.h>

#include "parray.hh"
#include "smobs.hh"

class Pdf_object
{
  DECLARE_SMOBS(Pdf_object,);
  bool written_;
  int object_number_;
  SCM value_;
  long byte_count_;


  static void write_dict (FILE*, SCM);
  static void write_stream (FILE*, SCM);
  static void write_vector (FILE*, SCM);
  static void typecheck (SCM);
  static String escape_string (String);

  String to_string () const;
  void write_to_file (FILE*, bool) const;
public:

  bool is_indirect () const;
  bool is_stream () const;
  bool is_dict () const;
  
  Pdf_object();
  void set_value (SCM value);

  friend class Pdf_file;
};

class Pdf_file
{
  Link_array<Pdf_object> indirect_objects_;
  Pdf_object *root_object_;
  FILE *file_;
  
  DECLARE_SMOBS(Pdf_file,);

protected:
  void write_header ();
  void write_trailer ();

  String get_string ();
  
public:
  void set_root_document (Pdf_object *obj);
  Pdf_file (String filename);
  void make_indirect (Pdf_object *obj);
  void write_object (Pdf_object *obj);
  void terminate();
};

DECLARE_UNSMOB(Pdf_object, pdf_object);
DECLARE_UNSMOB(Pdf_file, pdf_file);

#endif
