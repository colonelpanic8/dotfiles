CXX ?= c++
CXXFLAGS += -std=c++23 -fPIC -fno-gnu-unique -Wall -Wextra -Wno-unused-parameter
CPPFLAGS += $(shell pkg-config --cflags hyprland pixman-1 libdrm)

libhyprfocus.so: main.cpp FocusLayout.cpp FocusStrip.cpp FocusLayout.hpp FocusStrip.hpp
	$(CXX) $(CPPFLAGS) $(CXXFLAGS) -shared main.cpp FocusLayout.cpp FocusStrip.cpp -o $@

.PHONY: clean
clean:
	rm -f libhyprfocus.so
