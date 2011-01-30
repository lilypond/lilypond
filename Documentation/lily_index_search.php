<?php
  $languages = array ("en"=>"en", "de"=>"de", "nl"=>"nl", "ja"=>"ja", "hu"=>"hu", "fr"=>"fr", "zh"=>"zh", ""=>"en");
  $manuals = array ("essay"=>"essay", "extending"=>"extending", "learning"=>"learning", "notation"=>"notation", "usage"=>"usage");

  $lang = $languages[$_REQUEST['lang']];
  $man = $manuals[$_REQUEST['manual']];
  if (!$man) {
    echo "<p>Invalid manual " . $_REQUEST['lang'] . "</p>";
    exit ();
  }
  $bigpage = ($_REQUEST['bigpage'] == "1");
  $search_string = $_REQUEST['q'];
  // If enter was pressed, browsers will use the returned HTML for a complete page!
  $form_submitted = $_REQUEST['form_submitted'];


  $relpath = "";
  if ($form_submitted) {
    if (! $bigpage) {
      $relpath = "$man/";
    }
    echo "<html><body>\n";
  }

  $filename = "./$man";
  if ($bigpage) { $filename .= "-big-page"; }
  $filename .= ".$lang.idx";

  $found = 0;
  $file = @fopen($filename, "r");
  if ($file ) {
    while ( (($line=fgets($file)) !== false) ) {
      $line = rtrim($line);
      $entries = split ("\t", $line);
      if (stripos ($entries[0], $search_string) !== false) {
        if ($found == 0) {
          echo "<p><b>Search results for &quot;".htmlentities($search_string, ENT_QUOTES)."&quot;:</b><br>\n";
          echo "<table id=\"search_result_table\">\n";
        } else if ($found > 50) {
          echo "<tr><td colspan=2>Too many hits, displaying only 50 results</td></tr>\n";
          break;
        }
        // format the entry and print it out
        echo "<tr><td><a href=\"$relpath$entries[2]\">$entries[1]</a></td>\n";
        echo "    <td><a href=\"$relpath$entries[4]\">$entries[3]</a></td></tr>\n";
        $found++;
      }
    }
    if ($found > 0) {
      echo "</table>\n";
    } else {
      echo "No results found in the index.\n";
    }
    echo "</p>";
    fclose($file);
  } else {
    echo "<p>Unable to open search index $filename</p>";
  }
  if ($form_submitted) {
    echo "</body></html>\n";
  }
?>
