# ref-check.py
#
# This file is part of LilyPond, the GNU music typesetter.
#
# Copyright (C) 2010--2022 Trevor Daniels <t.daniels@treda.co.uk>
#
# LilyPond is free software: you can redistribute it and/or modify
# it under the terms of the GNU General Public License as published by
# the Free Software Foundation, either version 3 of the License, or
# (at your option) any later version.
#
# LilyPond is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
# GNU General Public License for more details.
#
# You should have received a copy of the GNU General Public License
# along with LilyPond.  If not, see <http://www.gnu.org/licenses/>.

"""
*** RefCheck

Flow
  Read files
    On @defManual manual refManual eg @defManual LM rlearning
      Associate manual name 'manual' with reference @'refManual'{ ... }
      Add 'refManual' to list of keywords
    On @setManual manual set current manual name to 'manual'
    On @include open and process new file
    On @node add node to Nodes list with current manual name and current file name
    On @ref add contents to References list with refManual, current manual name and current file name
    On one of refManual keywords add contents to References list with ditto
  Match refs and nodes
    Process References list:
      Check reference is contained in Nodes list with refManual in Nodes = refManual in References
    Print results

Known issues
  Node names containing commas are only checked up to the comma
  Spurious warnings "Xref should be internal" for files in /included

"""


##################################################
class CrossRefs:
    """ Holds References and Nodes """

    def __init__(self):
        self.Manuals = {}
        self.Nodes = {}
        self.nodeNames = {}  # used to check for duplicates only
        self.Refs = []
        self.nodeCount = 0

    def addManual(self, manualName, refManual):
        self.Manuals[refManual] = manualName

    def getRefManuals(self):
        return list(self.Manuals.keys())

    def getManualName(self, refManual):
        return self.Manuals[refManual]

    def addNode(self, nodeName, manualName, fileName):
        global returnCode
#    print "Node: ", nodeName, " in ", manualName, " found in ", fileName
        if "\\" in nodeName:
            returnCode = 1
            print("nodeName: ", nodeName, " in ",
                  fileName, " contains backslash")
        if manualName+"/"+nodeName in list(self.Nodes.keys()):
            print("Error: Duplicate nodename ", nodeName, " in ",
                  fileName, " and ", self.Nodes[manualName+"/"+nodeName][1])
            returnCode = 1
        self.Nodes[manualName + "/" + nodeName] = [manualName, fileName]
        self.nodeNames[nodeName] = fileName

    def addRef(self, toManualName, toHeading, inFileName):
        global returnCode
        if "\\" in toHeading:
            returnCode = 1
            print("ref to: ", toHeading, " in ",
                  inFileName, " contains backslash")
#    if inFileName == "notation/vocal.itely":
#      print "Ref to ", toManualName, "/",toHeading, " found in ", inFileName
        self.Refs.append([toManualName + "/" + toHeading, inFileName])

    def check(self):
        noErrors = True
        for [refHeading, refFileName] in self.Refs:
            try:
                targetFileName = self.Nodes[refHeading]
#        if refFileName == "notation/vocal.itely":
#          print "ref to: ", refHeading, " in ", refFileName, " found in ", targetFileName
            except KeyError:
                noErrors = False
                print("ref to: ", refHeading, " in ",
                      refFileName, " not found")
        if noErrors:
            print(" All references satisfied")
        else:
            returnCode = 1

##################################################


class File:
    """ Process an included file """

    # Class variables
    CurrentManualName = ""
    DefaultPath = ""
    Excludes = []
    Paths = {}

    # Methods
    def __init__(self, fileName):
        self.fileName = fileName
        try:
            self.fullFileName = File.Paths[fileName] + fileName
        except KeyError:
            self.fullFileName = File.DefaultPath + fileName

    def read(self, crossRefs):
        """ Process File """

        skip = False
        try:
            myfile = open(self.fullFileName, 'r', encoding='utf-8')
        except IOError:
            print("File ", self.fullFileName, " referenced in ",
                  File.CurrentManualName, " but not found")
            return
        remainderLine = ""
        lineNo = 0
        for line in myfile:
            lineNo += 1
            words = line.split()
            if len(words) > 0:
                if words[0] == "@ignore" or words[0] == "@macro":
                    skip = True
                if skip and len(words) > 1:
                    if words[0] == "@end" and (words[1].find("ignore") >= 0 or words[1].find("macro") >= 0):
                        skip = False

                if not skip and words[0] != "@c":
                    if words[0].find("@defManual") >= 0:
                        # Manual definition found - extract manual name and refManual string
                        manualName = words[1]
                        refManual = words[2]
                        crossRefs.addManual(manualName, refManual)
#            print manualName, refManual

                    elif words[0].find("@defaultPath") >= 0:
                        File.DefaultPath = words[1]

                    elif words[0].find("@path") >= 0:
                        File.Paths[words[1]] = words[2]

                    elif words[0].find("@setManual") >= 0:
                        File.CurrentManualName = words[1]
#            print " Checking ", File.CurrentManualName

                    elif words[0].find("@exclude") >= 0:
                        File.Excludes.append(words[1])

                    elif words[0].find("@include") >= 0:
                        if words[1] not in File.Excludes:
                            currentFileName = words[1]
#              print "  File: ", currentFileName
                            currentFile = File(currentFileName)
                            currentFile.read(crossRefs)

                    elif words[0] == "@node":
                        nodeName = line[6:-1]
                        crossRefs.addNode(
                            nodeName, File.CurrentManualName, self.fileName)

                    # Find references

                    twoLines = remainderLine + ' ' + line.strip()
                    manualRefStrings = crossRefs.getRefManuals()
                    refFound = False
                    for manualRefString in manualRefStrings:
                        toManualName = crossRefs.getManualName(manualRefString)
                        actualToManualName = toManualName
                        if toManualName == "this":
                            toManualName = File.CurrentManualName
                        refString = "@" + manualRefString + "{"
                        refStart = twoLines.find(refString)
                        if refStart >= 0:
                            refFound = True
                            if actualToManualName == File.CurrentManualName:
                                print("Warning: should xref be internal around line ",
                                      lineNo, " in ", self.fileName, "?")
                            twoLines = twoLines[refStart:]
                            refNodeStart = twoLines.find("{") + 1
                            # TODO Need to check here for nested {}
                            refNodeEnd = twoLines.find("}")
                            refNodeEndComma = twoLines.find(",")
                            if refNodeEndComma > 0:
                                refNodeEnd = min(refNodeEnd, refNodeEndComma)
                            if refNodeEnd >= 0:
                                crossRefs.addRef(
                                    toManualName, twoLines[refNodeStart:refNodeEnd], self.fileName)
                                remainderLine = twoLines[refNodeEnd+1:]
                        if refFound:
                            refFound = False
                            break
                    if not refFound:
                        remainderLine = ""

        myfile.close()
        return


topFile = File("scripts/auxiliar/ref_check.tely")  # TODO get from input params
print("RefCheck ver 0.1")
returnCode = 0
crossRefs = CrossRefs()
topFile.read(crossRefs)
crossRefs.check()
if returnCode > 0:
    print("Errors found: status code: ", returnCode)
