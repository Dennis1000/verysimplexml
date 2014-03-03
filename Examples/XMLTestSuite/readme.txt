Directory structure
===================
XMLTestSuite for Win32 (D2010) and Win64 (XE2+):
VCL\ 

XMLTestSuite for Win32/64, MacOS X (XE5+):
FMX-Desktop\   

XMLTestSuite for iOS/Android (XE5+):
FMX-Mobile\    

XMLTestSuite source code:
Source\

XML-TestFiles:
TestFiles\


XML test files
==============
The XML test files are split into groups, the file naming is as follows:

  test<GroupNr>v<TestNr>.xml

Each XML test has a corresponding *.out file, which contains the
supposed output after loading, parsing and saving. In addition there are 
these optional files:

*.txt - description of the xml test file
*.pibl - processing instructions before loading the xml file
*.pial - processing instructions after loading of the xml file


Processing instructions
=======================
Processing instructions files may contain one ore more of these commands
(each command on one line, any spaces before the command are ignored):

compact=1                  sets the [doCompact] options
indent="[indent]"          sets the NodeIndentStr to [indent] (quotes will be omitted)
linebreak=unix|windows     sets the line break to unix style (#$0A) or to windows style (#$0D#$0A)
encoding=<encstr>          sets the xml declaration encoding to <encstr>

findnode=<nodename>        finds a node with the name <nodename>, same as .FindNode('<nodename>')
addnode=<nodename>         adds a new node to the current node
settext=<text>             sets the value of the current node to <text>
istext=<text>              compares the text of the current node with <text>
addattribute=<attribute>   adds a new attribute <attribute> to the current node
setvalue=<attribvalue>     sets the value of the current attribute to <attribvalue>
parent                     sets the current node to the current nodes' parent  

