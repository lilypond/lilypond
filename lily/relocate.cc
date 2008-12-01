/*
  relocate.cc -- implement relocation based on argv0

  source file of the GNU LilyPond music typesetter

  (c) 2005--2008 Han-Wen Nienhuys <hanwen@xs4all.nl>

*/

#include "relocate.hh"

#include "config.hh"


/* TODO: autoconf support */

#include <sys/types.h>
#include <dirent.h>

#if HAVE_GETTEXT
#include <libintl.h>
#endif

#include "file-name.hh"
#include "file-path.hh"
#include "international.hh"
#include "lily-guile.hh"
#include "lily-version.hh"
#include "main.hh"
#include "version.hh"
#include "warn.hh"

#define FRAMEWORKDIR ".."

int
sane_putenv (char const *key, string value, bool overwrite)
{
  if (overwrite || !getenv (key))
    {
      string combine = string (key) + "=" + value;
      char *s = strdup (combine.c_str ());

      if (be_verbose_global)
	progress_indication (_f ("Setting %s to %s" , key, value.c_str ())
			     + "\n");
			     
      int retval =  putenv (s);
      /*
	unfortunately, we can't portably free S here,
	due to various bugs in glibc prior to 2.1.1
      */ 
      return retval;
    }
  
  return -1;
}

static int
set_env_file (char const *key, string value, bool overwrite = false)
{
  if (is_file (value))
    return sane_putenv (key, value, overwrite);
  else if (be_verbose_global)
    warning (_f ("no such file: %s for %s", value, key));
  return -1;
}

static int
set_env_dir (char const *key, string value)
{
  if (is_dir (value))
    return sane_putenv (key, value, false);
  else if (be_verbose_global)
    warning (_f ("no such directory: %s for %s", value, key));
  return -1;
}

static int
prepend_env_path (char const *key, string value)
{
  if (is_dir (value))
    {
      if (be_verbose_global)
	progress_indication (_f ("%s=%s (prepend)\n", key, value.c_str ())); 

      if (char const *cur = getenv (key))
	value += to_string (PATHSEP) + cur;

      return sane_putenv (key, value.c_str (), true);
    }
  else if (be_verbose_global)
    warning (_f ("no such directory: %s for %s", value, key));
  return -1;
}

#ifdef __MINGW32__
#include <winbase.h>
#endif

static void
prefix_relocation (string prefix)
{
  string bindir = prefix + "/bin";
  string datadir = prefix + "/share";
  string localedir = datadir + "/locale";
  string package_datadir = datadir + "/lilypond/";
  string old_lilypond_datadir = lilypond_datadir;

  if (is_dir (package_datadir + "/" + TOPLEVEL_VERSION))
    lilypond_datadir = package_datadir + "/" + TOPLEVEL_VERSION;
  else if (is_dir (package_datadir + "/current"))
    lilypond_datadir = package_datadir + "/current";
  else
    warning (_f ("not relocating, no %s/ or current/ found under %s",
		 TOPLEVEL_VERSION, package_datadir.c_str ()));
  
#if HAVE_GETTEXT
  if (is_dir (localedir))
    bindtextdomain ("lilypond", localedir.c_str ());
#endif

  prepend_env_path ("PATH", bindir);

  if (be_verbose_global)
    warning (_f ("Relocation: compile datadir=%s, new datadir=%s",
		 old_lilypond_datadir.c_str (),
		 lilypond_datadir.c_str ()));
}

/*
  UGH : this is a complete mess.
*/

static void
framework_relocation (string prefix)
{
  if (be_verbose_global)
    warning (_f ("Relocation: framework_prefix=%s", prefix));

  sane_putenv ("INSTALLER_PREFIX", prefix, true);
	       
  read_relocation_dir (prefix + "/etc/relocate/");

  string bindir = prefix + "/bin";
  
  prepend_env_path ("PATH", bindir);
}

/*
  UGH : this is a complete mess.
*/
void
setup_paths (char const *argv0_ptr)
{
  File_name argv0_filename (argv0_ptr);
  
  if (relocate_binary)
    {
      string prefix_directory;
      if (getenv ("LILYPOND_RELOCATE_PREFIX"))
	{
	  prefix_directory = getenv ("LILYPOND_RELOCATE_PREFIX");
#ifdef __MINGW32__
	  /* Normalize file name.  */
	  prefix_directory = File_name (prefix_directory).to_string ();
#endif /* __MINGW32__ */
      
	  prefix_relocation (prefix_directory);
	  string bindir = prefix_directory + "/bin";
	  framework_relocation (bindir);
	}
      else if (relocate_binary)
	{
	  string argv0_abs;
	  if (argv0_filename.is_absolute ())
	    {
	      argv0_abs = argv0_filename.to_string ();
	      if (be_verbose_global)
		warning (_f ("Relocation: is absolute: argv0=%s", argv0_ptr));
	    }
	  else if (argv0_filename.dir_.length ())
	    {
	      argv0_abs = get_working_directory ()
		+ "/" + string (argv0_filename.to_string ());
	      if (be_verbose_global)
		warning (_f ("Relocation: from cwd: argv0=%s", argv0_ptr));
	    }
	  else
	    {
	      /* Find absolute ARGV0 name, using PATH.  */
	      File_path path;
	      path.parse_path (getenv ("PATH"));

	      if (be_verbose_global)
		warning (_f ("Relocation: from PATH=%s\nargv0=%s",
			     path.to_string ().c_str (), argv0_ptr));

#ifndef __MINGW32__
	      argv0_abs = path.find (argv0_filename.to_string ());
#else /* __MINGW32__ */
	      char const *ext[] = {"exe", "", 0 };
	      argv0_abs = path.find (argv0_filename.to_string (), ext);
#endif /* __MINGW32__ */

	      if (argv0_abs.empty ())
		programming_error ("cannot find absolute argv0");
	    }

	  string bindir = dir_name (argv0_abs);
	  string argv0_prefix = dir_name (bindir);
	  string compile_prefix = dir_name (dir_name (dir_name (lilypond_datadir)));
	  if (argv0_prefix != compile_prefix)
	    {
	      prefix_relocation (argv0_prefix);
	      prefix_directory = argv0_prefix;
	    }
	  if (argv0_prefix != compile_prefix || string (FRAMEWORKDIR) != "..")
	    {
	      framework_relocation (bindir + "/" + FRAMEWORKDIR);
	      prefix_directory = bindir + "/" + FRAMEWORKDIR;
	    }
	}

      lilypond_datadir = prefix_directory
	+ "/share/lilypond/" TOPLEVEL_VERSION;
    }

  if (getenv ("LILYPONDPREFIX"))
    error (_ ("LILYPONDPREFIX is obsolete, use LILYPOND_DATADIR"));

  if (char const *env = getenv ("LILYPOND_DATADIR"))
    {
      /* Normalize file name.  */
      lilypond_datadir = File_name (env).to_string ();
    }

  /* When running from build dir, a full LILYPOND_DATADIR is set-up at
     $(OUTBASE)/{share, lib}/lilypond/current.  Configure lily using
     ./configure --prefix=$(pwd)/out */
  string build_datadir_current = dir_name (lilypond_datadir) + "/current";
  if (!is_dir (lilypond_datadir.c_str ())
      && is_dir (build_datadir_current.c_str ()))
    lilypond_datadir = build_datadir_current;


  lilypond_datadir = File_name (lilypond_datadir).canonicalized().to_string();
  
  global_path.append ("");

  /* Adding mf/out make lilypond unchanged source directory, when setting
     LILYPONDPREFIX to lilypond-x.y.z */
  char const *suffixes[] = {"ly", "ps", "scm", 0 };
  
  vector<string> dirs;
  for (char const **s = suffixes; *s; s++)
    {
      string path = lilypond_datadir + to_string ('/') + string (*s);
      dirs.push_back (path);
    }
  
  dirs.push_back (lilypond_datadir + "/fonts/otf/");
  dirs.push_back (lilypond_datadir + "/fonts/type1/");
  dirs.push_back (lilypond_datadir + "/fonts/svg/");
  
  for (vsize i = 0; i < dirs.size (); i++)
    global_path.prepend (dirs[i]);
}

string
expand_environment_variables (string orig)
{
  const char *start_ptr = orig.c_str ();
  const char *ptr = orig.c_str ();
  size_t len = orig.length ();

  string out;
  while (ptr < start_ptr + len)
    {
      char *dollar = strchr (ptr, '$');
      
      if (dollar != NULL)
	{
	  char *start_var = dollar + 1;
	  char *end_var = start_var;
	  char *start_next = end_var;
	  
	  out += string (ptr, dollar - ptr);
	  ptr = dollar;

	  if (*start_var == '{')
	    {
	      start_var ++;
	      
	      end_var = strchr (start_var, '}');
	      
	      if (end_var == NULL)
		{
		  end_var = start_var + len;
		  start_next = end_var;
		}
	      else
		{
		  start_next = end_var + 1; 
		}
	    }
	  else 
	    {
	      /*
		Hmm. what to do for $1 , $~ etc.?
	      */
	      do
		{
		  end_var ++;
		}
	      while (isalnum (*end_var) || *end_var == '_');
	      start_next = end_var;
	    }

	  if (start_var < end_var)
	    {
	      string var_name (start_var, end_var - start_var);
	      const char *value = getenv (var_name.c_str ());
	      if (value != NULL)
		out += string (value);

	      ptr = start_next;
	    }
	}
      else
	break;

    }

  out += ptr;

  return out;
}

// Ugh - very inefficient, but safer than fgets.
static string
read_line (FILE *f)
{
  string out;
  
  int c = 0;
  while ((c = fgetc (f)) != EOF && c != '\n')
    out += char(c);

  return out;
}

void
read_relocation_file (string filename)
{
  if (be_verbose_global)
    progress_indication (_f ("Relocation file: %s", filename.c_str ())
			 + "\n");
      
  char const *cname = filename.c_str ();
  FILE *f = fopen (cname, "r");
  if (!f)
    error (_f ("cannot open file: `%s'", cname));

  while (!feof (f))
    {
      string line = read_line (f);
      size_t idx = line.find (' ');
      if (idx == NPOS)
	continue;
      
      string command = line.substr (0, idx);
      line = line.substr (idx + 1);
      
      if (idx == NPOS)
	continue;
      idx = line.find ('=');

      string variable = line.substr (0, idx);
      string value = line.substr (idx + 1);

      value = expand_environment_variables (value);

      if (command == "set")
	sane_putenv (variable.c_str (), value, true);
      else if (command == "setdir")
	set_env_dir (variable.c_str (), value);
      else if (command == "setfile")
	set_env_file (variable.c_str (), value);
      else if (command == "prependdir")
	prepend_env_path (variable.c_str (), value);
      else
	error (_f ("Unknown relocation command %s", command));
    }

  fclose (f);
}

void
read_relocation_dir (string dirname)
{
  if (DIR *dir = opendir (dirname.c_str ()))
    while (struct dirent *ent = readdir (dir))
      {
	File_name name (ent->d_name);
	if (name.ext_ == "reloc")
	  read_relocation_file (dirname + "/" + name.to_string ());
      }
}
