# Makefile for 
# Copyright (C) 2013 Randall Gray May
# $Header$
# $Log$

CC = gcc
CXX = g++

#CPPFLAGS =

CFLAGS = -g
CXXFLAGS = -g

#LDFLAGS =
#LDLIBES =
#LDLIBS = 

#TEX = latex
#TEXI2DVI = texi2dvi

OBJS =
SRCS = $(OBJS:.o=.c)

# "make SO=dylib" for OS X
# TODO: use auto-detection from chibi's Makefile
SO ?= so



igor: igor.c local/csupport.sld local/csupport.$(SO)
	gcc -ggdb -DDEBUGGING -Wall -o igor igor.c -I/opt/local/include -L/usr/local/lib -L/opt/local/lib -lreadline -lhistory -lchibi-scheme
	chmod a+rx igor

install:
	sudo mv /bin/igor /tmp/igor
	sudo cp -p igor /bin/igor
	sudo chown root.root /bin/igor 

install-links:
	echo I know how to do it for my Gentoo machine, but elsewhere?
	echo ... for me it is 
	echo '    ln -s ~/igor/local /usr/local/lib64/chibi/'

local: igor.c local/csupport.sld local/csupport.$(SO)
	gcc -ggdb -DDEBUGGING -Wall -o igor igor.c -L/usr/local/lib -lreadline -lhistory -lchibi-scheme
	chmod a+rx igor

local/csupport.$(SO): local/external-support.c local/csupport.sld local/csupport.stub
	make -C local clean csupport.$(SO)
	make

clean:
	make -C local clean 
	rm -f igor *.o
