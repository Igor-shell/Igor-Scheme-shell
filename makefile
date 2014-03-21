# Makefile for 
# Copyright (C) 2013 Randall Gray May
# $Header$
# $Log$

#OPTLIBDIR = /opt/lib
#OPTLOCALLIBDIR = /opt/local/lib
LOCALLIBDIR = /usr/local/lib
CHIBILIBDIR = /usr/local/lib/chibi
CHIBISHAREDIR = /usr/local/lib/chibi

#OPTINCDIR = /opt/include
#OPTLOCALINCDIR = /opt/local/include
LOCALINCDIR = /usr/local/include
CHIBIINCDIR = /usr/local/include/chibi


 
DEBUG = -ggdb -DDEBUGGING -Wall 
#DEBUG = -ggdb -DDEBUGGING

CC = gcc
#CC = clang
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



igor: igor.c es.sld es.$(SO) 
	gcc $(DEBUG) -o igor igor.c -I/opt/local/include  -L$(LOCALLIBDIR) -lreadline -lhistory -lchibi-scheme -lexternal-support
	chmod a+rx igor

libexternal-support.$(SO): external-support.c external-support.h
	gcc $(DEBUG) -fPIC -shared -Wall  -o libexternal-support.$(SO) external-support.c -I$(LOCALINCDIR) -lchibi-scheme -lreadline -lhistory

es.$(SO):	es.stub es.sld external-support.o 
	chibi-ffi es.stub
	gcc $(DEBUG) -fPIC -shared es.c -o es.$(SO) -lchibi-scheme -lreadline -lhistory

install: igor
	sudo mv /bin/igor /tmp/igor || true
	sudo cp -p igor /bin/igor
	sudo chown root.root /bin/igor 

install-links:
	echo "I know how to do it for my Gentoo machine, but elsewhere?\n"

install-lib: libexternal-support.$(SO)
	sudo install -D --group=root --owner=root --mode=755 libexternal-support.so $(LOCALLIBDIR)

install-links-gentoo: install-lib install
	sudo mkdir -p $(CHIBILIBDIR)/local $(CHIBILIBDIR)/local
	sudo chmod a+rx $(CHIBILIBDIR)/local $(CHIBILIBDIR)/local
	sudo install -D --group=root --owner=root --mode=755 es.so $(CHIBILIBDIR)/local
	sudo install -D --group=root --owner=root --mode=755 es.sld es.scm $(CHIBISHAREDIR)/local

# for local modifications
install-links-no-really: install-lib install
	sudo mkdir -p $(CHIBILIBDIR)/local $(CHIBILIBDIR)/local
	sudo chmod a+rx $(CHIBILIBDIR)/local $(CHIBILIBDIR)/local
	sudo install -D --group=root --owner=root --mode=755 es.so $(CHIBILIBDIR)/local
	sudo install -D --group=root --owner=root --mode=755 es.sld es.scm $(CHIBISHAREDIR)/local

install-gentoo: install-links-gentoo install
	echo Installed

clean:
	rm -f igor *.o *.so



tester: tester.c local/csupport.sld local/csupport.$(SO)
	gcc -ggdb -DDEBUGGING -Wall -o tester tester.c -I/opt/local/include -L$(LOCALLIBDIR) -L/opt/local/lib -lreadline -lhistory -lchibi-scheme

