/*
   path.cc - manipulation of paths and filenames.
*/
#include <stdio.h>
#include "path.hh"
#include "flower-debug.hh"

#ifndef PATHSEP
#define PATHSEP '/'
#endif

/**
   @param path the original full filename
   @return 4 components of the path. They can be empty
*/
void
split_path(String path, 
	   String &drive, String &dirs, String &filebase, String &extension)
{
    // peel off components, one by one.
    int di = path.index_i(':');
    if (di >= 0) 
	{
	drive = path.left_str(di + 1);
	path = path.right_str(path.len() - di -1);
	} 
    else
	drive = "";
    
    di = path.index_last_i(PATHSEP);
    if (di >=0) 
	{
	dirs = path.left_str(di + 1);
	path = path.right_str(path.len()-di -1);
	}
    else
	dirs = "";
    
    di = path.index_last_i('.');
    if (di >= 0) 
	{
	filebase = path.left_str(di);
	extension =path.right_str(path.len()-di);	
	} 
    else 
	{
	extension = "";   
	filebase = path;
	}
}

/** find a file. 
  It will search in the current dir, in the construction-arg, and
  in any other added path, in this order.
  */
String
File_path::find(String nm)const

{
    fdebug << "looking for " << nm << ": ";
    if ( !nm.length_i() || ( nm == "-" ) )
	return nm;
    for (int i=0; i < size(); i++) {

	 String path  = (*this)[i];
	 path+= String(path.length_i()? "/":"")+nm;

	 fdebug << path << "? ";
	 FILE *f = fopen(path, "r"); // ugh!
	 if (f) {
	     fdebug << "found\n";
	     fclose(f);
	     return path;
	 }
     }
    fdebug << "\n";
    return "";
}
