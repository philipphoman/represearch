#!/bin/bash

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


if [ -e $LATEXNEW ]; then
		echo "Removing macro ..."
    if [ -e $MODDIR/basic.bak ]; then
        cp -rl $MODDIR/basic $MODDIR/basic.old && rm -r $MODDIR/basic
        cp -rl $MODDIR/basic.bak $MODDIR/basic && rm -r $MODDIR/basic.bak
				rm -r $MODDIR/basic.old
		    #mv $MODDIR/basic $MODDIR/basic.old
		    #mv $MODDIR/basic.bak $MODDIR/basic
		fi
else
		echo "Macro not installed - exiting ..."
fi

