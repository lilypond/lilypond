/*
  source.cc -- implement Sources

  source file of the LilyPond music typesetter

  (c) 1997--2008 Han-Wen Nienhuys <hanwen@xs4all.nl>
*/

#include "sources.hh"

#include "config.hh"
#include "source-file.hh"
#include "file-name.hh"
#include "file-path.hh"

Sources::Sources ()
{
  path_ = 0;
}


Sources::Sources (Sources const &)
{
  assert (false);
}


void
Sources::set_path (File_path *f)
{
  path_ = f;
}

/**
   Open a file. If the name is not absolute, look in CURRENT_DIR first.
   Afterwards, check the rest of the path_.

   FILE_STRING the name of the file to be opened.
   CURRENT_DIR a path to a directory, either absolute or relative to the
     working directory.
*/
Source_file *
Sources::get_file (string file_string, string const& current_dir)
{  
  if (file_string != "-")
    {
      // First, check for a path relative to the directory of the
      // file currently being parsed.
      if (current_dir.length ()
	  && file_string.length ()
	  && !File_name (file_string).is_absolute ()
	  && is_file (current_dir + DIRSEP + file_string))
	file_string = current_dir + DIRSEP + file_string;

      // Otherwise, check the rest of the path.
      else if (path_)
	{
	  string file_string_o = path_->find (file_string);
	  if ((file_string_o == "") && (file_string != ""))
	    return 0;

	  file_string = file_string_o;
	}
    }

  Source_file *f = new Source_file (file_string);
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

