cmake_minimum_required(VERSION 3.27)

project(hypr-workspace-history
    DESCRIPTION "Workspace history cycling plugin for Hyprland"
    VERSION 0.1.0
    LANGUAGES CXX
)

set(CMAKE_CXX_STANDARD 23)
set(CMAKE_CXX_STANDARD_REQUIRED ON)

add_library(hypr-workspace-history SHARED src/main.cpp)

find_package(PkgConfig REQUIRED)
pkg_check_modules(deps REQUIRED IMPORTED_TARGET
    hyprland
    wayland-server
    xkbcommon
)

target_link_libraries(hypr-workspace-history PRIVATE rt PkgConfig::deps)

install(TARGETS hypr-workspace-history)
