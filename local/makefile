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
SO ?= so

csupport.$(SO): csupport.stub external-support.c external-chibi.c
	chibi-ffi csupport.stub
	gcc -fPIC -shared csupport.c -o csupport.$(SO) -lchibi-scheme -lreadline -lhistory


clean:
	rm -f csupport.c csupport.$(SO)
