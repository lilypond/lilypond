/*
  source.cc -- implement Sources

  source file of the LilyPond music typesetter

  (c) 1997--2006 Han-Wen Nienhuys <hanwen@xs4all.nl>
*/

#include "source.hh"

#include "source-file.hh"
#include "file-path.hh"

Sources::Sources ()
{
  path_ = 0;
  is_binary_ = false;
}

void
Sources::set_binary (bool bo)
{
  is_binary_ = bo;
}

void
Sources::set_path (File_path *f)
{
  path_ = f;
}

/**
   open a file

   File_string the file to be opened, name might be changed if it is
   found in a search path.
*/
Source_file *
Sources::get_file (string *file_string) //UGH
{
  if (*file_string != "-" && path_)
    {
      string file_string_o = path_->find (*file_string);
      if ((file_string_o == "") && (*file_string != ""))
	return 0;
      *file_string = file_string_o;
    }
  Source_file *f = new Source_file (*file_string);
  add (f);
  return f;
}

void
Sources::add (Source_file *sourcefile)
{
  sourcefiles_.push_back (sourcefile);
}

Sources::~Sources ()
{
  for (vsize i = 0; i < sourcefiles_.size (); i++)
    {
      sourcefiles_[i]->unprotect ();
    }
}

Source_file *
Sources::get_sourcefile (char const *str0)
{
  for (vector<Source_file*>::iterator i = sourcefiles_.begin();
       i != sourcefiles_.end (); i++)
    {
      if ((*i)->contains (str0))
	return *i;
    }

  return 0;
}

