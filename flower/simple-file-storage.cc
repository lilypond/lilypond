/*
  simple-file-storage.cc -- implement Simple_file_storage

  source file of the GNU LilyPond music typesetter

  (c)  1997--2000 Han-Wen Nienhuys <hanwen@cs.uu.nl>
*/

#include <stdio.h>

#ifndef SEEK_END
#define SEEK_END 2
#endif

#include "simple-file-storage.hh"
#include "array.hh"
#include "string.hh"
#include "warn.hh"

void
Simple_file_storage::load_stdin ()
{
  len_ = 0;

  int c;
  Array<char> chs;
  while ((c = fgetc (stdin)) != EOF)
    chs.push (c);
  len_ = chs.size ();
  data_ = chs.remove_array ();
}

void
Simple_file_storage::load_file (String s)
{
  /*
    let's hope that "b" opens anything binary, and does not apply
    CR/LF translation
    */
  FILE * f =  fopen (s.to_str0 (), "rb");

  if (!f)
    {
      warning (_f ("can't open file: `%s'", s));
      return ;
    }

  int ret = fseek (f, 0, SEEK_END);
  len_ = ftell (f);
  rewind (f);
  data_ = new char[len_+1];
  data_[len_] = 0;
  ret = fread (data_, sizeof (char), len_, f);

  if (ret!=len_)
    warning (_f ("Huh?  Got %d, expected %d characters", ret, len_));

  fclose (f);
}

/**
  Stupid but foolproof way of opening files.

  TODO
  Should check IO status

  This is of course a build it yourself version of mmap, so we should
  have been using that..., but this is simple & portable
  
*/

Simple_file_storage::Simple_file_storage (String s)
{
  data_ = 0;
  len_ = 0;

  if ((s == "-"))
    load_stdin ();
  else
    load_file (s);
}

char const*
Simple_file_storage::to_str0 () const
{
  return data_;
}

int
Simple_file_storage::length () const
{
  return len_;
}


Simple_file_storage::~Simple_file_storage ()
{
  delete []data_;
}
