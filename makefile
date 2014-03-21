# Makefile for 
# Copyright (C) 2013 Randall Gray May
# $Header$
# $Log$

LOCALIBDIR = /usr/local/lib
CHIBILIBDIR = /usr/local/lib/chibi
CHIBISHAREDIR = /usr/local/lib/chibi

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



igor: igor.c es.sld es.$(SO) libexternal-support.so
	gcc $(DEBUG) -o igor igor.c -I/opt/local/include external-support.o -L/usr/local/lib -L/opt/local/lib -lreadline -lhistory -lchibi-scheme -lexternal-support
	chmod a+rx igor

libexternal-support.$(SO): external-support.c external-support.h
	gcc $(DEBUG) -fPIC -shared -Wall  -o libexternal-support.$(SO) external-support.c -I/opt/local/include  -lchibi-scheme -lreadline -lhistory

es.$(SO):	es.stub es.sld external-support.o 
	chibi-ffi es.stub
	gcc $(DEBUG) -fPIC -shared es.c -o es.$(SO) -lchibi-scheme -lreadline -lhistory

install: igor
	sudo mv /bin/igor /tmp/igor || true
	sudo cp -p igor /bin/igor
	sudo chown root.root /bin/igor 

install-links:
	echo "I know how to do it for my Gentoo machine, but elsewhere?\n"

install-links-gentoo:
	sudo install -D --group=root --owner=root --mode=755 libexternal-support.so $(LIBDIR)
	sudo install -D --group=root --owner=root --mode=755 es.so $(CHIBILIBRDIR)/chibi/local
	sudo install -D --group=root --owner=root --mode=755 es.sld es.scm $(CHIBISHAREDIR)/chibi/local

install-links-no-really: install
	sudo install -D --group=root --owner=root --mode=755 libexternal-support.so /usr/local/lib
	sudo install -D --group=root --owner=root --mode=755 es.so /usr/local/lib/chibi/local
	sudo install -D --group=root --owner=root --mode=755 es.sld es.scm /usr/local/share/chibi/local

clean:
	rm -f igor *.o *.so



tester: tester.c local/csupport.sld local/csupport.$(SO)
	gcc -ggdb -DDEBUGGING -Wall -o tester tester.c -I/opt/local/include -L/usr/local/lib -L/opt/local/lib -lreadline -lhistory -lchibi-scheme

