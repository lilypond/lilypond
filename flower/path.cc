/*
   path.cc - manipulation of paths and filenames.
*/
#include <stdio.h>
#include "path.hh"

#ifndef PATHSEP
#define PATHSEP '/'
#endif


void
split_path(String path, 
	   String &drive, String &dirs, String &filebase, String &extension)
{
    // peel off components, one by one.
    int di = path.pos(':');
    if (di) 
	{
	drive = path.left(di);
	path = path.right(path.len() - di);
	} 
    else
	drive = "";
    
    di = path.lastPos(PATHSEP);
    if (di) 
	{
	dirs = path.left(di);
	path = path.right(path.len()-di);
	}
    else
	dirs = "";
    
    di = path.lastPos('.');
    if (di) 
	{
	di --; // don't forget '.'
	filebase = path.left(di);
	extension =path.right(path.len()-di);	
	} 
    else 
	{
	extension = "";   
	filebase = path;
	}
}
/**
   INPUT: path the original full filename
   OUTPUT: 4 components of the path. They can be empty
*/


File_path::File_path(String pref)
{
    add(".");
    add(pref);
}


///
String
File_path::find(String nm)
{
     for (int i=0; i < size(); i++) {
	 String path  = (*this)[i];
	 path+= "/"+nm;


	 FILE *f = fopen(path, "r"); // ugh!
	 if (f) {
	     fclose(f);
	     return path;
	 }
     }
     return "";
}
/**
  It will search in the current dir, in the construction-arg, and
  in any other added path, in this order.
  */
