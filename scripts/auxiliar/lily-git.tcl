#!/usr/bin/wish

# GUI interface for common LilyPond git repository commands
# Copyright 2009 by Johannes Schindelin and Carl Sorensen
#

set version 0.59

# set to 1 to set up for translation, to 0 for other
set translator 0

if {$translator == 1} {
        set windowTitle \
          "LilyPond Translator's Git Interface version $version"
        set updateButtonText "1. Update translation"
        set initializeButtonText "1. Get translation"
        set originHead "lilypond/translation"
        set rebase 0
} else {
        set windowTitle \
          "LilyPond Contributor's Git Interface version $version"
        set updateButtonText "1. Update source"
        set initializeButtonText "1. Get source"
        set originHead "master"
        set rebase 1
}
package require Tk

# Helper functions

set lily_dir $env(HOME)/lilypond
if {[file exists $lily_dir]} {
	cd $lily_dir
}

set abort_dir "./aborted_edits"

proc write_to_output {s} {
	.output.text insert insert $s
	.output.text see end
}

proc write_file_to_output {f} {
	if {[eof $f]} {
		global git_command
		fconfigure $f -blocking true
		if {[catch {close $f} err]} {
			tk_messageBox -type ok -message \
                          "Command returned an error: $err\n\nCheck output text for details"
		}
		unset git_command
	} else {
		write_to_output [read $f 24]
	}
}

proc git {args} {
	global lily_dir git_command
	set git_command [linsert $args 0 "|git" "--git-dir=$lily_dir/.git"]
	set git_command "$git_command 2>@1"
	.output.text insert end "$git_command\n"
	set git [open $git_command r]
	fconfigure $git -blocking false
	fileevent $git readable [list write_file_to_output $git]
	vwait git_command
}

proc config {args} {
	global lily_dir
	set p [open [linsert $args 0 "|git" --git-dir=$lily_dir/.git config] r]
	set result [regsub "\n\$" [read $p] ""]
	if {[catch {close $p} err]} {
		tk_messageBox -type ok -message "config failed: $err"
	}
	return $result
}

proc config_quiet {args} {
	global lily_dir
	set p [open [linsert $args 0 "|git" --git-dir=$lily_dir/.git config] r]
	set result [regsub "\n\$" [read $p] ""]
	if {[catch {close $p} err]} {
		set result ""
	}
	return $result
}

proc update_lilypond_rebase {} {
  update_lilypond 1
}

proc commit {} {
  global commit_message
  global commit_canceled
  set commit_canceled 0
  get_commit_message
  tkwait visibility .commitMessage
  tkwait window .commitMessage
  if {$commit_canceled != 1} {
    if {$commit_message == ""} {
      tk_messageBox -message "You must enter a commit message!" \
      -type ok -icon error
    } else {
      git commit -a -m $commit_message
      git rebase --whitespace=fix HEAD^
    set commit_message ""
    }
  }
}

# This won't work, because --amend needs an editor
#  lilyconfig users are on their own.
proc commit_amend {} {
  git commit -a --amend
  git rebase --whitespace=fix HEAD^
}

proc update_lilypond_norebase {} {
  update_lilypond 0
}

proc update_lilypond_with_rebase {} {
  global rebase
  update_lilypond $rebase
}

proc update_lilypond {rebase} {
	global lily_dir
        global originHead
        global translator
	. config -cursor watch
	if {![file exists $lily_dir]} {
		write_to_output "Cloning LilyPond (this can take some time) ...\n"
		file mkdir $lily_dir
		cd $lily_dir
		git init
		git config core.bare false
		git remote add -t $originHead \
			origin git://git.sv.gnu.org/lilypond.git
                if {$translator == 1} {
                  git fetch
                } else {
                  git fetch --depth 1
                }
		git reset --hard origin/$originHead
		git config branch.$originHead.remote origin
		git config branch.$originHead.merge refs/heads/$originHead
                .buttons.commit configure -state normal
                .buttons.update configure -text buttonUpdateText
                .buttons.patch configure -state normal
                .buttons.panic configure -state normal
                toggle_rebase
	} else {
		write_to_output "Updating LilyPond...\n"
		git fetch origin
		if {$rebase} {
			git rebase origin/$originHead
		} else {
			git merge origin/$originHead
		}
	}
	write_to_output "Done.\n"
	. config -cursor ""
}

proc patch_from_origin {} {
  global rebase
  make_patch_from_origin $rebase
  if {![llength [glob -nocomplain 0*.patch]]} {
	tk_messageBox -type ok -message \
		"No patches created; did you make a local commit?"
  }
}

proc make_patch_from_origin {rebase} {
  global lily_dir
  global originHead
  . config -cursor watch
  update_lilypond $rebase
  write_to_output "Creating patch...\n"
  git format-patch origin/$originHead
  write_to_output "Done.\n"
  . config -cursor ""
}

proc abort_changes {} {
  global abort_dir
  global originHead
  set answer [tk_messageBox -type okcancel \
               -message "This will copy all changed files to $abort_dir and reset the repository." \
               -default cancel]
  switch -- $answer {
    ok {
      write_to_output "abort_dir: $abort_dir \n"
      if {![file exists $abort_dir]} {
        set return_code [exec mkdir $abort_dir]
      }
      set return_code [catch {exec git diff origin/$originHead} gitdiff]
      set return_code [regexp {diff --git a/(\S*)} $gitdiff match modified_file]
      while {$return_code != 0} {
        write_to_output "Copying $modified_file to $abort_dir.\n"
        set return_code [catch {exec cp $modified_file $abort_dir} result]
        set return_code [regsub {diff --git a/(\S*)} $gitdiff "" gitdiff]
        set return_code [regexp {diff --git a/(\S*)} $gitdiff match modified_file]
      }
      set return_code [git reset --hard origin/$originHead]
      write_to_output "Repository reset. \n"
    }
  }
}

proc toggle_rebase {} {
	global rebase
        global lily_dir
        global originHead
        global updateButtonText
        global initializeButtonText
        if {[file exists $lily_dir]} {
	  config --bool branch.$originHead.rebase $rebase
          .buttons.update configure -text $updateButtonText
        } else {
          .buttons.update configure -text $initializeButtonText
        }
}

proc clear_rebase {} {
  global rebase
  set rebase 0
  toggle_rebase
}

proc set_rebase {} {
  global rebase
  set rebase 1
  toggle_rebase
}

proc commitMessageOK {} {
  global commit_message
  global commit_header
  set commit_body [.commitMessage.bottomFrame.commit_body get 1.0 end]
  set commit_message "$commit_header\n\n$commit_body"
  destroy .commitMessage
}

proc commitMessageCancel {} {
  global commit_message
  global commit_canceled
  set commit_message ""
  set commit_canceled 1
  destroy .commitMessage
}


# Commit message input window
proc get_commit_message {} {
  global commit_header
  set commit_header ""
  toplevel .commitMessage
  frame .commitMessage.topFrame
  label .commitMessage.topFrame.label \
      -text "Enter commit message header:\n(50 chars max = width of box)"
  entry .commitMessage.topFrame.commit_header \
            -width 50 -relief solid -border 2 -textvariable commit_header
  pack   .commitMessage.topFrame.label -side left
  pack   .commitMessage.topFrame.commit_header -side left

  frame  .commitMessage.bottomFrame
  text   .commitMessage.bottomFrame.commit_body \
            -width 75  -height 10 -relief solid -border 2 -wrap none

  frame .commitMessage.bottomFrame.leftFrame
  label .commitMessage.bottomFrame.leftFrame.label \
      -text "Enter commit message body:\n(No limit -- Full description)"
  button .commitMessage.bottomFrame.leftFrame.ok \
           -text OK -default active -command commitMessageOK
  button .commitMessage.bottomFrame.leftFrame.cancel -text Cancel -default active \
          -command commitMessageCancel
  wm withdraw .commitMessage
  wm title .commitMessage "Git Commit Message"

  pack .commitMessage.bottomFrame.leftFrame.label
  pack .commitMessage.bottomFrame.leftFrame.ok
  pack .commitMessage.bottomFrame.leftFrame.cancel

  pack .commitMessage.bottomFrame.leftFrame -side left
  pack .commitMessage.bottomFrame.commit_body -side left

  pack .commitMessage.topFrame
  pack .commitMessage.bottomFrame

  wm transient .commitMessage .
  wm deiconify .commitMessage
}


# GUI

wm title . $windowTitle

# Buttons

panedwindow .buttons
button .buttons.commit -text "2. Make local commit" -command commit
button .buttons.update -text $updateButtonText \
          -command update_lilypond_with_rebase
toggle_rebase
button .buttons.patch -text "3. Make patch set" \
          -command patch_from_origin
button .buttons.panic -text "Abort changes -- Reset to origin" \
          -command abort_changes -fg Blue -bg Red
label   .buttons.spacer -text "                         "
if {![file exists $lily_dir]} {
	.buttons.update configure \
            -text $initializeButtonText
        .buttons.commit configure -state disabled
        .buttons.patch configure -state disabled
        .buttons.panic configure -state disabled
}

#  Operating buttons

pack .buttons.update -side left
pack .buttons.commit -side left
pack .buttons.patch -side left
pack .buttons.spacer -side left
pack .buttons.panic -side left


# Output text box

panedwindow .output
label .output.label -text "Command output:"
text .output.text -width 80 -height 15 \
	-xscrollcommand [list .output.horizontal set] \
	-yscrollcommand [list .output.vertical set] \
        -relief solid -border 2
scrollbar .output.horizontal -orient h -command [list .output.text xview]
scrollbar .output.vertical -orient v -command [list .output.text yview]
pack .output.label -side left
pack .output.horizontal -side bottom -fill x
pack .output.vertical -side right -fill y
pack .output.text -expand true -anchor nw -fill both

pack .buttons
pack .output

#grid .buttons -row 2 -column 1
#grid .output -row 3 -column 1 -sticky "w"
