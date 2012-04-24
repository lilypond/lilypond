#!/usr/bin/wish

# GUI interface for common LilyPond git repository commands
# Copyright 2009--2012 by Johannes Schindelin and Carl Sorensen
#
package require Tk

set version 0.66

proc get_environment_var {var_name default_value} {
    global env
    if [catch {set return_value $env($var_name)}] {
        set return_value $default_value
    }
    return $return_value
}
# set to 1 to set up for translation, to 0 for other
set translator 0

# If you have push access, set to 1, or use LILYPOND_GIT_PUSH
set default_push_access 0
set push_access [get_environment_var "LILYPOND_GIT_PUSH" $default_push_access]


# location of lilypond git
set lily_dir [get_environment_var LILYPOND_GIT $env(HOME)/lilypond-git]

if {$translator == 1} {
    set windowTitle \
        "LilyPond Translator's Git Interface version $version"
    set updateButtonText "1. Update translation"
    set initializeButtonText "1. Get translation"
    set originHead "translation"
    set pushHead $originHead
    set rebase 0
} else {
    set windowTitle \
        "LilyPond Contributor's Git Interface version $version"
    set updateButtonText "1. Update source"
    set initializeButtonText "1. Get source"
    set originHead "master"
    set pushHead "staging"
    set defaultBranch "dev/local_working"
    set rebase 1
}


##  Submits the user data collected using the git config command

proc submituserdata {} {
    exec git config --global user.name "$::username"
    exec git config --global user.email "$::useremail"
    destroy .b
    return 0
}

##  Request name and email from user

proc requestuserdata {} {
    toplevel .b
    grab .b
    wm geometry .b -300-300
    wm title .b "Contributor details"
    grid [frame .b.c ] -column 0 -row 0 -sticky nwes
    grid columnconfigure . 0 -weight 1; grid rowconfigure . 0 -weight 1

    grid [entry .b.c.username -width 20 -textvariable username] -column 2 -row 2 -sticky we
    grid [entry .b.c.useremail -width 20 -textvariable useremail] -column 2 -row 3 -sticky we
    grid [button .b.c.submituserdata -text "Submit" -command submituserdata] -column 2 -row 4

    grid [label .b.c.explbl -text "Please enter your name and email for future commits:"] -column 1 -row 1 -columnspan 3 -sticky we
    grid [label .b.c.nmlbl -text "Name:"] -column 1 -row 2  -sticky w
    grid [label .b.c.emlbl -text "Email:"] -column 1 -row 3 -sticky w

    foreach w [winfo children .b.c] {grid configure $w -padx 5 -pady 5}
    focus .b.c.username
    bind .b <Return> {submituserdata}
}

##  Checks the user's global .gitconfig for name and email and executes requestuserdata if either is not found

if {![file exists "$env(HOME)/.gitconfig"]} {
    set fileholder [open "$env(HOME)/.gitconfig" a+]
} else {
    set fileholder [open "$env(HOME)/.gitconfig" r]
}

set usercheck [split [read $fileholder] "\n"]
close $fileholder
if {![regexp "name" $usercheck] || ![regexp "email" $usercheck]} then {
    requestuserdata
    tkwait window .b
}

##  Entry limit routine from jeff at hobbs org, downloaded from
##  http://www.purl.org/net/hobbs/tcl/tclet/entrylimit.html

## For those who aren't using Tk4 with these, make a bell noop:
if [string match {} [info commands bell]] { proc bell args {} }

proc forceLen {len name el op} {
    global $name ${name}_len
    if [string comp $el {}] {
        set old  ${name}_len\($el)
        set name $name\($el)
    } else {
        set old ${name}_len
    }
    if {[string length [set $name]] > $len} {
        set $name [set $old]
        bell;
        return
    }
    set $old [set $name]
}

## Here is a wish example to use the routines.  Remember that with
## write traces, a valid value must be set for each variable both
## before AND after the trace is established.

## The order must be:
## 1) variable init
## 1) textvariable specification
## 3) set trace
## 4) variable reinit

set commit_header {}
trace variable commit_header w {forceLen 50}
set commit_header {}


## End of entry limit code


# Helper functions

proc add_working_branch {} {
    global originHead
    global workingBranch
    git checkout $originHead
    git branch -f $workingBranch
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

proc commit_amend {} {
    git commit -a --amend -C HEAD
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
    global pushHead
    global translator
    global workingBranch
    global push_access
    . config -cursor watch
    if {![file exists $lily_dir]} {
        write_to_output "Cloning LilyPond (this can take some time) ...\n"
        file mkdir $lily_dir
        cd $lily_dir
        git init
        git config core.bare false
        git remote add -t $originHead \
            origin git://git.sv.gnu.org/lilypond.git
        git fetch
        git reset --hard origin/$originHead
        git config branch.$originHead.remote origin
        git config branch.$originHead.merge refs/heads/$originHead
        git checkout $originHead
        if {$workingBranch != ""} {
            add_working_branch
            git checkout $workingBranch
        }
        .buttons.commitFrame.commit configure -state normal
        .buttons.commitFrame.amend configure -state normal
        .buttons.update configure -text buttonUpdateText
        .buttons.patch configure -state normal
        if {$push_access && !$translator} {
            .buttons.push configure -state normal
        }
        .buttons.panic configure -state normal
        toggle_rebase
    } else {
        write_to_output "Updating LilyPond...\n"
        git fetch origin
        if {$rebase} {
            git rebase origin/$originHead $originHead
            git rebase origin/$originHead $workingBranch
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


proc push_patch_to_staging {} {
    global workingBranch
    global pushHead
    global git_log
    global push_canceled
    global log_text
    global originHead

    git rebase $originHead $workingBranch
    set staging_sha [exec git rev-parse ]
    set head_sha [exec git rev-parse $workingBranch]
    set log_error [catch {exec git --no-pager log {--pretty=format:%h : %an -- %s} --graph $originHead..$workingBranch} log_text]
    if {$log_error == 0 && $log_text == ""} {
        tk_messageBox -type ok -message "No changes in repository.  Nothing to push."
    } else {
        get_git_log
        tkwait visibility .gitLogWindow
        tkwait window .gitLogWindow
        if {$push_canceled == 0} {
            git rebase origin/$pushHead $workingBranch~0
            git push origin HEAD:$pushHead
            git checkout $workingBranch
        }
    }
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

proc pushContinue {} {
  global push_canceled
  set push_canceled = 0
  destroy .gitLogWindow
}

proc pushCancel {} {
    global push_canceled
    set push_canceled 1
    destroy .gitLogWindow
}


# git log output window
proc get_git_log {} {
    global log_text
    toplevel .gitLogWindow
    frame  .gitLogWindow.messageFrame


    text   .gitLogWindow.messageFrame.message_body \
       -xscrollcommand [list .gitLogWindow.messageFrame.horizontal set] \
       -yscrollcommand [list .gitLogWindow.messageFrame.vertical set] \
         -width 60  -height 10 -relief solid -border 2 -wrap none
    scrollbar .gitLogWindow.messageFrame.horizontal -orient h -command [list .gitLogWindow.messageFrame.message_body xview]
    scrollbar .gitLogWindow.messageFrame.vertical -orient v -command [list .gitLogWindow.messageFrame.message_body yview]

    frame .gitLogWindow.messageFrame.leftFrame
    label .gitLogWindow.messageFrame.leftFrame.label \
        -text "Log of commits in push:"
    button .gitLogWindow.messageFrame.leftFrame.ok \
        -text Continue -default active -command pushContinue
    button .gitLogWindow.messageFrame.leftFrame.cancel -text Cancel -default active \
        -command pushCancel
    wm withdraw .gitLogWindow
    wm title .gitLogWindow "Commits to be pushed"

    pack .gitLogWindow.messageFrame.leftFrame.label
    pack .gitLogWindow.messageFrame.leftFrame.ok
    pack .gitLogWindow.messageFrame.leftFrame.cancel

    pack .gitLogWindow.messageFrame.leftFrame -side left

    pack .gitLogWindow.messageFrame.horizontal -side bottom -fill x
    pack .gitLogWindow.messageFrame.vertical -side right -fill y
    pack .gitLogWindow.messageFrame.message_body -expand true -anchor nw -fill both
    pack .gitLogWindow.messageFrame

    wm transient .gitLogWindow .
    wm deiconify .gitLogWindow
    .gitLogWindow.messageFrame.message_body insert insert $log_text
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

frame  .buttons.commitFrame
button .buttons.commitFrame.commit -text "2a. New local commit" -command commit
button .buttons.commitFrame.amend -text "2b. Amend previous commit" -command commit_amend
pack .buttons.commitFrame.commit -fill x
pack .buttons.commitFrame.amend -fill x

button .buttons.update -text $updateButtonText \
    -command update_lilypond_with_rebase
button .buttons.patch -text "3. Make patch set" \
    -command patch_from_origin
if {$push_access && !$translator} {
    button .buttons.push -text "4. Push patch to staging" \
    -command push_patch_to_staging
}
toggle_rebase
button .buttons.panic -text "Abort changes -- Reset to origin" \
    -command abort_changes -fg Blue -bg Red
label   .buttons.spacer -text "                         "
if {![file exists $lily_dir]} {
    .buttons.update configure \
        -text $initializeButtonText
    .buttons.commitFrame.commit configure -state disabled
    .buttons.commitFrame.amend configure -state disabled
    .buttons.patch configure -state disabled
    if {$push_access} {
        .buttons.push configure -state disabled
    }
    .buttons.panic configure -state disabled
}

#  Operating buttons

pack .buttons.update -side left
pack .buttons.commitFrame -side left
pack .buttons.patch -side left
if {$push_access} {
    pack .buttons.push -side left
}
pack .buttons.spacer -side left
pack .buttons.panic -side right


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

# set working branch and push branch
set workingBranch [get_environment_var LILYPOND_BRANCH $defaultBranch]

puts "\nworkingBranch $workingBranch\n"

if {[file exists $lily_dir]} {
    cd $lily_dir
    set branchList  [exec git branch]
    if { $workingBranch != ""} {
        if {![regexp $workingBranch $branchList]} {
            add_working_branch
        }
        git checkout $workingBranch
    }
}

