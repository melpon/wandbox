cmake_minimum_required(VERSION 3.15)

project(kennel C CXX)

set(Boost_USE_STATIC_LIBS ON)

find_package(Boost REQUIRED
  COMPONENTS
    program_options)

# ---- cattleshed

add_executable(cattleshed
  src/server.cc
  src/load_config.cc
  src/quoted_printable.cc
  src/syslogstream.cc)
set_target_properties(cattleshed PROPERTIES CXX_STANDARD 14 C_STANDARD 99)
target_compile_definitions(cattleshed PRIVATE SYSCONFDIR=${CMAKE_INSTALL_PREFIX}/etc)
target_link_libraries(cattleshed
  Boost::boost
  Boost::program_options)

# ---- cattlegrid

add_executable(cattlegrid
  src/jail.cc)
set_target_properties(cattlegrid PROPERTIES CXX_STANDARD 14 C_STANDARD 99)
target_link_libraries(cattlegrid
  Boost::boost
  Boost::program_options)

# ---- prlimit

add_executable(prlimit
  src/prlimit.cc)
set_target_properties(prlimit PROPERTIES CXX_STANDARD 14 C_STANDARD 99)

# ---- インストール

