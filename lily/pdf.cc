/*
  pdf.cc --  implement Pdf output routines.

  source file of the GNU LilyPond music typesetter

  (c) 2005 Han-Wen Nienhuys <hanwen@xs4all.nl>

*/

#include "warn.hh"
#include "pdf.hh"
#include "ly-smobs.icc"

IMPLEMENT_SMOBS(Pdf_object);
IMPLEMENT_DEFAULT_EQUAL_P(Pdf_object);
IMPLEMENT_TYPE_P(Pdf_object, "pdf-object?");


Pdf_object::Pdf_object ()
{
  object_number_ = 0;
  value_ = SCM_BOOL_F;
  written_ = false;
  byte_count_ = 0;
  
  smobify_self ();
}


bool
Pdf_object::is_dict () const
{
  return scm_is_pair (value_) && scm_car (value_) == ly_symbol2scm ("dictionary");
}

bool
Pdf_object::is_indirect () const
{
  return object_number_ > 0;
}

Pdf_object::~Pdf_object()
{
  
}

void
Pdf_object::typecheck (SCM val)
{
  if (scm_is_pair (val))
    {
      SCM tag = scm_car (val);
      if (tag == ly_symbol2scm ("null"))
	val = SCM_UNDEFINED;
      else if (tag == ly_symbol2scm ("dictionary"))
	{
	  SCM alist = scm_cdr (val);
	  for (SCM s = alist; scm_is_pair (alist); s = scm_cdr (s))
	    {
	      Pdf_object *key = unsmob_pdf_object (scm_caar (s));
	      Pdf_object *val = unsmob_pdf_object (scm_cdar (s));

	      if (!key || !val)
		{
		  ly_display_scm (scm_car (s));
		  error ("key and value must be PDF objects.");
		}
	    }
	}
      else if (tag == ly_symbol2scm ("stream"))
	{
	  if (!scm_is_string (scm_cdr (val)))
	    error ("stream argument should be string");	      
	}
      else
	{
	  ly_display_scm (tag);
	  error ("unknown tag");
	}
    }
  else if (scm_is_vector (val))
    {
      SCM vec = val;
      int len = scm_c_vector_length (vec);
      for (int i = 0; i < len; i++)
	{
	  Pdf_object *val = unsmob_pdf_object (scm_c_vector_ref (vec, i));
	  if (!val)
	    {
	      ly_display_scm (scm_c_vector_ref (vec, i));
	      error ("array content should be PDF object");
	    }
	}
    }
}

void
Pdf_object::set_value (SCM val)
{
  if (written_)
    error ("Can't set value for written PDF object");

  typecheck (val);

  value_ = val; 
}

SCM
Pdf_object::mark_smob (SCM smob)
{
  Pdf_object *p = (Pdf_object*) SCM_CELL_WORD_1 (smob);
  return p->value_; 
}

int
Pdf_object::print_smob (SCM smob, SCM port, scm_print_state *)
{
  Pdf_object *obj = (Pdf_object*) SCM_CELL_WORD_1 (smob);
  scm_puts ("#<Pdf_object ", port);
  scm_display (obj->value_, port);
  scm_puts (">", port);
  return 1;
}


void
Pdf_object::write_dict (FILE *file, SCM alist)
{
  String str = "";

  fputs ("<< " , file);
  for (SCM s = alist; scm_is_pair (s); s = scm_cdr (s))
    {
      Pdf_object *key = unsmob_pdf_object (scm_caar (s));
      Pdf_object *val = unsmob_pdf_object (scm_cdar (s));

      assert (val && key);
	  
      key->write_to_file (file, false);
      val->write_to_file (file, false);
    }
  fputs (">>\n" , file);
}

void
Pdf_object::write_vector (FILE *file, SCM vec)
{
  String str = "";

  fputs ("[ " , file);
  int len = scm_c_vector_length (vec);
  for (int i = 0; i < len; i++)
    {
      Pdf_object *val = unsmob_pdf_object (scm_c_vector_ref (vec, i));
      assert (val);
      val->write_to_file (file, false);
    }
  fputs ("]\n" , file);
}


void 
Pdf_object::write_stream (FILE *file, SCM scmstr)
{
  String str = ly_scm2string (scmstr);
  fprintf (file, "<< /Length %d >>\nstream\n" , str.length());
  fwrite (str.get_bytes(), str.length(), sizeof(Byte), file);
  fputs ("endstream" , file);
}

String
Pdf_object::escape_string (String str)
{
  str.substitute_char ('\\', "\\\\");
  str.substitute_char ('(', "\\(");
  str.substitute_char (')', "\\)");
  return str;
}

String
Pdf_object::to_string () const
{
  if (value_ == SCM_BOOL_F)
    return "false";
  else if (value_ == SCM_UNDEFINED)
    return "null";
  else if (value_ == SCM_BOOL_T)
    return "true";
  else if (scm_is_integer (value_))
    return ::to_string (scm_to_int (value_));
  else if (scm_is_number (value_))
    return ::to_string (scm_to_double (value_));
  else if (scm_is_symbol (value_))
    return "/" + ly_symbol2string (value_) ;
  else if (scm_is_string (value_))
    return "(" + escape_string (ly_scm2string (value_)) + ")";

  assert (false);
}

void
Pdf_object::write_to_file (FILE* file, bool dump_definition) const
{
  if (is_indirect () && !dump_definition)
    {
      fprintf (file, "%d 0 R", object_number_);
      return;
    }
  
  if (scm_is_vector (value_))
    write_vector (file, value_);
  else if (scm_is_pair (value_))
    {
      SCM tag = scm_car (value_);
      if (tag == ly_symbol2scm ("dictionary"))
	{
	  write_dict (file, scm_cdr (value_));
	}
      else if (tag == ly_symbol2scm ("stream"))
	{
	  write_stream (file, scm_cdr (value_));
	}
      else
	{
	  assert (false);
	}
    }
  else
    {
      String str = to_string ();
      fwrite (str.get_bytes(), str.length(), sizeof(Byte), file);
      
      fputc (dump_definition ? '\n' : ' ', file);
      return ;
    }
}

bool
Pdf_object::is_stream () const
{
  return scm_is_pair (value_) && scm_car (value_) == ly_symbol2scm ("stream");
}

/****************************************************************/
  

IMPLEMENT_SMOBS(Pdf_file);
IMPLEMENT_DEFAULT_EQUAL_P(Pdf_file);
IMPLEMENT_TYPE_P(Pdf_file, "pdf-file?");


Pdf_file::Pdf_file (String name)
{
  char const *cp = name.to_str0 ();
  root_object_ = NULL;
  file_ = fopen  (cp, "w");
  if (!file_)
    {
      error (_f ("Can't open file %s", cp));
    }
  write_header ();
  smobify_self ();
}

void
Pdf_file::write_header ()
{
  fputs ("%PDF-1.3\n", file_);
}

void
Pdf_file::make_indirect (Pdf_object *obj)
{
  assert (!obj->is_indirect());

  /*
    Skip 0 , the null object.
   */
  obj->object_number_ = indirect_objects_.size() + 1;
  indirect_objects_.push (obj);
}

void
Pdf_file::write_object (Pdf_object *obj)
{
  assert (!obj->written_);
  if (obj->is_stream() && !obj->is_indirect ())
    {
      make_indirect (obj);
    }
  
  if (obj->is_indirect())
    {
      obj->byte_count_ = ftell (file_);
      fprintf (file_, "%d obj\n", obj->object_number_);
    }
  
  obj->write_to_file (file_, true);
  obj->written_ = true;

  if (obj->is_indirect ())
    {
      fprintf (file_, " \nendobj\n");
    }
}

void
Pdf_file::terminate ()
{
  for (int i = 0; i < indirect_objects_.size (); i++)
    {
      if (!indirect_objects_[i]->written_)
	write_object (indirect_objects_[i]);
    }
  
  write_trailer ();
  fclose (file_);
  file_ = NULL;
}

Pdf_file::~Pdf_file()
{
}


void
Pdf_file::set_root_document (Pdf_object*obj)
{
  if (root_object_)
    {
      error ("Can have only one root object");
    }
  
  root_object_ = obj;
  if (!obj->is_indirect ())
    {
      make_indirect (obj);
    }  
}

void
Pdf_file::write_trailer ()
{
  long xref_offset = ftell (file_);
  fprintf (file_, "xref\n%d %d\n", 0, indirect_objects_.size() + 1);

  char const *xref_entry = "%010d %05d %c \n";
  fprintf (file_, xref_entry, 0, 65535, 'f');
  for (int i = 0; i < indirect_objects_.size(); i++)
    {
      fprintf (file_, xref_entry, indirect_objects_[i]->byte_count_, 0, 'n');
    }

  fprintf (file_, "trailer\n<< /Size %d /Root %d 0 R >>",
	   indirect_objects_.size () + 1, (root_object_) ? root_object_->object_number_ : 0);
  fprintf (file_, "\nstartxref\n%d\n", xref_offset);
  fputs ("%%EOF", file_);
}


SCM
Pdf_file::mark_smob (SCM f)
{
  Pdf_file *pfile = (Pdf_file*) SCM_CELL_WORD_1(f);
  for (int i = 0; i < pfile->indirect_objects_.size(); i++)
    scm_gc_mark (pfile->indirect_objects_[i]->self_scm());
  return SCM_BOOL_F;
}

int
Pdf_file::print_smob (SCM pdf, SCM port, scm_print_state*)
{
  scm_puts ("#<PDF file>", port);
  return 1;
}
