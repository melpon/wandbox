.PHONY:
.PHONY: all clean

CXXFLAGS = -std=c++0x -g  -fmax-errors=2 -Wall -Wextra

all: server.exe client.exe ptracer.exe threadtest.exe

server.exe: server.o
	g++ -g -o$@ $^ -lboost_system -lpthread -lrt

client.exe: client.o
	g++ -g -o$@ $^ -lboost_system -lpthread -lrt

ptracer.exe: ptracer.o
	g++ -g -o$@ $^

threadtest.exe: threadtest.o
	g++ -g -o$@ $^ -lpthread -lrt

server.cc: common.hpp
client.cc: common.hpp
