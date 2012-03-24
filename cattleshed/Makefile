.PHONY:
.PHONY: all clean

CXXFLAGS = -std=c++0x -g -I$(HOME)/repos/boost -fmax-errors=2 -Wall -Wextra

all: server.exe client.exe

server.exe: server.o
	g++ -g -o$@ $^ -lboost_system -lpthread -lrt

client.exe: client.o
	g++ -g -o$@ $^ -lboost_system -lpthread -lrt


server.cc: common.hpp
client.cc: common.hpp

