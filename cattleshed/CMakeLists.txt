cmake_minimum_required(VERSION 3.15)

project(kennel C CXX)

set(Boost_USE_STATIC_LIBS ON)

find_package(Boost REQUIRED
  COMPONENTS
    program_options)
find_package(RT REQUIRED)
find_package(CAP REQUIRED)
find_package(Threads REQUIRED)

# ---- 初期値

if(NOT CATTLESHED_BINDIR)
  set(CATTLESHED_BINDIR ${CMAKE_INSTALL_PREFIX}/bin)
endif()

if(NOT CATTLESHED_SYSCONFDIR)
  set(CATTLESHED_SYSCONFDIR ${CMAKE_INSTALL_PREFIX}/etc)
endif()

if(NOT CATTLESHED_STOREDIR)
  set(CATTLESHED_STOREDIR "/var/log/wandbox/ran")
endif()

if(NOT CATTLESHED_BASEDIR)
  set(CATTLESHED_BASEDIR "/tmp/wandbox")
endif()

if(NOT CATTLESHED_LISTEN_PORT)
  set(CATTLESHED_LISTEN_PORT 2012)
endif()

if(NOT CATTLESHED_SERVICE_ARGS)
  set(CATTLESHED_SERVICE_ARGS "-c /opt/wandbox/cattleshed-conf/compilers.default")
endif()

# ---- cattleshed

add_executable(cattleshed
  src/server.cc
  src/load_config.cc
  src/quoted_printable.cc
  src/syslogstream.cc)
set_target_properties(cattleshed PROPERTIES CXX_STANDARD 14 C_STANDARD 99)
target_compile_definitions(cattleshed PRIVATE SYSCONFDIR="${CMAKE_INSTALL_PREFIX}/etc")
target_link_libraries(cattleshed
  Boost::boost
  Boost::program_options
  RT::RT
  Threads::Threads)

# ---- cattlegrid

add_executable(cattlegrid
  src/jail.cc)
set_target_properties(cattlegrid PROPERTIES CXX_STANDARD 14 C_STANDARD 99)
target_link_libraries(cattlegrid
  Boost::boost
  Boost::program_options
  CAP::CAP
  Threads::Threads)

# ---- prlimit

add_executable(prlimit
  src/prlimit.cc)
set_target_properties(prlimit PROPERTIES CXX_STANDARD 14 C_STANDARD 99)

# ---- 静的ファイルの生成

set(CATTLESHED_CONF ${CMAKE_CURRENT_BINARY_DIR}/cattleshed.conf)
set(_CATTLESHED_CONF_IN ${PROJECT_SOURCE_DIR}/cattleshed.conf.in)
configure_file(${_CATTLESHED_CONF_IN} ${CATTLESHED_CONF} @ONLY)

set(CATTLESHED_SERVICE ${CMAKE_CURRENT_BINARY_DIR}/cattleshed.service)
set(_CATTLESHED_SERVICE_IN ${PROJECT_SOURCE_DIR}/cattleshed.service.in)
configure_file(${_CATTLESHED_SERVICE_IN} ${CATTLESHED_SERVICE} @ONLY)

# ---- インストール

install(TARGETS cattleshed cattlegrid prlimit)
install(FILES ${CATTLESHED_CONF} ${CATTLESHED_SERVICE} DESTINATION etc)
