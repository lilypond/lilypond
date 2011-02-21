% This template file was written by Phil Holmes to replace the one used for the bibliogrphy
% on the LilyPond project.  18 Feb 2011

% This section lists the possible fields, although not all are handled by the template
ENTRY
  { abstract
    address
    author
    booktitle
    chapter
    comment
    earlier
    edition
    editor
    howpublished
    institution
    journal
    key
    keyword
    later
    month
    note
    number
    organization
    pages
    private
    publisher
    school
    series
    title
    type
    URL
    volume
    year
  }
  {}
  { label extra.label sort.label }

% s and t came from the original.  The other strings are self explanatory
STRINGS { s t }
STRINGS { ThesisType ReportType }

% from the original
INTEGERS { nameptr namesleft numnames }

% This was simply copied and pasted from the original
FUNCTION {format.names}
{ 's :=
  #1 'nameptr :=
  s num.names$ 'numnames :=
  numnames 'namesleft :=
    { namesleft #0 > }
    % This line was changed: the {ff{ } } entry outputs the first name without allowing bibtex to use
    % a ~ to replace the spaces where it thinks fit.  The downside is that we lose hyphens...
    { s nameptr "{ff{ } }{vv }{ll}{, jj}" format.name$ 't :=
      nameptr #1 >
	{ namesleft #1 >
	    { ", " * t * }
	    { numnames #2 >
		{ "," * }
		'skip$
	      if$
	      t "others" =
		{ " et al." * }
		{ " and " * t * }
	      if$
	    }
	  if$
	}
	't
      if$
      nameptr #1 + 'nameptr :=
      namesleft #1 - 'namesleft :=
    }
  while$
}

FUNCTION {format.authors}
% This is a very common construct.  It means "if the author is empty, process the lines in the first pair
% of braces {}.  Otherwise process the lines in the second pair.  So if it's empty, an empty string is
% stacked.
{ author empty$
    { "" }
    % If it's not empty, it formats author (and stacks the result) and then checks whether editor is empty
    { author format.names
      editor empty$
        % If editor is empty, it appends an empty string (the * is a postfix operator meaning append this
        % string to the top entry on the stack
        { "" * }
        % Otherwise it appends a period, parenthesis.
        { ". (" * }
      if$
    }
  if$
}

FUNCTION {format.editor}
{ editor empty$
    { "" }
    { editor format.names
      editor num.names$ #1 >
	{ ", editors" * }
	{ ", editor" * }
      if$
      author empty$
        { "" * }
        { ")" * }
      if$
    }
  if$

}

FUNCTION {output.bibitem}
{ newline$
  "@item " write$ newline$
}

FUNCTION {format.edition}
{ edition empty$
    { "" }
    { edition " edition, " * }
  if$
}

FUNCTION {format.date}
{ year empty$
    { month empty$
	{ "" }
	{ "there's a month but no year in " cite$ * warning$
	  month
	}
      if$
    }
    { month empty$
	'year
	{ month " " * year * }
      if$
    }
  if$
}

FUNCTION {format.address}
{ address empty$
    { "" }
    { address
    year empty$
      { "" * }
      { ", " * }
    if$
    }
  if$
}

FUNCTION {format.publisher}
{
  publisher empty$
    { "" }
    { publisher
      address empty$
        { year empty$
            { "" * }
            { ", " * }
          if$
        }
        { ", " * }
      if$
    }
  if$
}

FUNCTION {format.note}
{ note empty$
    { "" }
    { note
      add.period$
    }
  if$
}

FUNCTION {format.title}
{ title empty$
    { "" }
    { "@strong{" title * "}" *
    journal empty$
    {
      volume empty$
        { "" * }
        { ", " * }
      if$
    }
    { "" * }
    if$
  }
  if$
}

FUNCTION {format.number}
{
  % ReportType is set by us when we call the correct function for the bibliography type
  ReportType empty$
  {
    volume empty$
      {number empty$
        { "" }
        { "Number " number * ". " * }
      if$
      }
      { "" }
    if$
    }
    { ReportType
      number empty$
        { ", " * }
        { " " * number * ", " * institution * ", " * }
      if$
    }
  if$
}

FUNCTION {format.volume}
{ journal empty$
  {
    volume empty$
      { "" }
      { "volume " volume * }
    if$
  }
  { "" }
  if$
}

FUNCTION {format.journal}
{ journal empty$
  { "" write$ }
  { "@emph{" journal * "}" *
    write$
    volume empty$
      { "" }
      { ", " volume * }
    if$
    write$
    number empty$
      { "" }
      { "(" number * ")" * }
    if$
    write$
    pages empty$
      { "" }
      { ":" pages * }
    if$
    write$
    year empty$
      { "" }
      { ", " }
    if$
    write$
 }
  if$
}

FUNCTION {format.series}
{ series empty$
    { "" }
    { series
      % This is an inbuilt function that adds a period when appropriate.
      add.period$
      " " *
    }
  if$
}

FUNCTION {format.booktitle}
{ booktitle empty$
    { "" }
    { "In @emph{" booktitle * "}, " *
      pages empty$
        { "" * }
        { "pages " * pages * ", " * }
      if$
    }
  if$
}

FUNCTION {format.thesis}
{ ThesisType empty$
    { "" }
    { ThesisType school * ", " *}
  if$
}

% This is the function that runs the formatting code.  It's called when the biblio type is @Book
FUNCTION {book}
{ output.bibitem
  format.authors write$
  format.editor write$
  "." write$ newline$
  format.title write$
  format.volume write$
  "." write$ newline$
  format.number write$
  format.journal
  format.series write$
  format.publisher write$
  format.address write$
  format.edition write$
  format.booktitle write$
  format.thesis write$
  format.date write$
  "." write$ newline$
  format.note write$
  newline$
  newline$
  newline$
  newline$
}

% All the following functions are called for the other biblio types. This is @article
FUNCTION {article}
{
  % And we just call the book function
  book
}

FUNCTION {booklet}
{
  book
}

FUNCTION {PhDThesis}
{
  % This sets the string ThesisType to be equal to "PhD Thesis"
  "PhD thesis, " 'ThesisType :=
  book
  % And this sets it back to an empty string
  "" 'ThesisType :=
}

FUNCTION {TechReport}
{
  "Technical Report" 'ReportType :=
  book
  "" 'ReportType :=
}

FUNCTION {InProceedings}
{
  book
}

FUNCTION {MastersThesis}
{
  "Master's thesis, " 'ThesisType :=
  book
  "" 'ThesisType :=
}

FUNCTION {InBook}
{
  book
}

% Haven't dug too deeply into this, but it seems to set the sort key
FUNCTION {presort}
{ cite$
  #1 entry.max$ substring$
  'sort.key$ :=
}

% The entry point for processing

% Read the .bib file
READ

% Work through the entries, calling the presort function
ITERATE {presort}

% Sort the entries
SORT

% This could go higher with all the other functions, but it _has_ to be above the call
FUNCTION {begin.bib}
{ "@c bib -> itexi intro" write$ newline$
  "@itemize" write$ newline$
}

% Run the begin.bib function
EXECUTE {begin.bib}

% For each bibliogrpahy entry call the relevant function (e.g. book, article, booklet, etc.)
ITERATE {call.type$}

FUNCTION {end.bib}
{ newline$
  "@end itemize" write$ newline$
  "@c bib -> itexi end" write$ newline$
}

EXECUTE {end.bib}
% And that's it.
