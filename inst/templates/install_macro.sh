#!/bin/bash
# The problem is that even though LibreOffice is invisible when started,
# it becomes visible after opening a document. There is a solution at
# https://forum.openoffice.org/en/forum/viewtopic.php?f=5&t=22548: Run
# LibreOffice headless to call a macro.
#
# The command line call should not specify the document to open, just a
# macro.
#
# For example (using the newer macro syntax): soffice -headless
# -invisible "vnd.sun.star.script:Standard.Module1.MySubroutine?
# language=Basic&location=application"
#
# The macro calls loadComponentFromUrl with the Hidden property set to
# true. This will cause the document to not become visible. Now the
# macro performs whatever it was going to do with the document.
#
# EDIT: To make it work for different files, pass the filename as a
# parameter using the older macro syntax. An example from
# https://forum.openoffice.org/en/forum/viewtopic.php?f=20&t=8232:
# soffice
# "macro:///Library3.Module1.test_Args(arg1,123,4.567,2000-12-31)"


MODDIR="/home/homan/.config/libreoffice/4/user/"
DIALOG=$MODDIR/basic/dialog.xlc 
DIALOGNEW=$MODDIR/basic/dialognew.xlc 
SCRIPT=$MODDIR/basic/script.xlc 
SCRIPTNEW=$MODDIR/basic/scriptnew.xlc 
LATEXNEW=$MODDIR/basic/LaTeX


REPLACESTR='s/<\/library:libraries>/<!--<\/library:libraries>-->/g' 

MACROSTR1="<library:library library:name=\"LaTeX\" \
xlink:href=\"\$(USER)/basic/LaTeX/dialog.xlb/\" xlink:type=\"simple\" \
library:link=\"false\"/> </library:libraries>"

MACROSTR2="<library:library library:name=\"LaTeX\" \
xlink:href=\"\$(USER)/basic/LaTeX/script.xlb/\" xlink:type=\"simple\" \
library:link=\"false\"/> </library:libraries>"


if [ ! -e $LATEXNEW ]; then

		echo "Installing macro ..."
		
		# backup basic
    if [ -e $MODDIR/basic.bak ]; then
		   mv $MODDIR/basic.bak /tmp/
		fi
		cp -a $MODDIR/basic $MODDIR/basic.bak
		cp -a LaTeX $LATEXNEW

		# backup files
		cp $DIALOG $DIALOG.bak
		cp $SCRIPT $SCRIPT.bak

		# Remove library closing tag
		sed -e $REPLACESTR  $DIALOG > $DIALOGNEW
		echo $MACROSTR1 >> $DIALOGNEW
		sed -e $REPLACESTR  $SCRIPT > $SCRIPTNEW
		echo $MACROSTR2 >> $SCRIPTNEW

		mv $DIALOGNEW $DIALOG
		mv $SCRIPTNEW $SCRIPT
else
		echo "Macro installed - exiting ..."
fi

