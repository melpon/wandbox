cmake_minimum_required(VERSION 3.15)

project(kennel C CXX)

set(Boost_USE_STATIC_LIBS ON)

find_package(Boost REQUIRED
  COMPONENTS
    program_options)
find_package(RT REQUIRED)
find_package(CAP REQUIRED)
find_package(Threads REQUIRED)
find_package(Protobuf REQUIRED)
find_package(gRPC REQUIRED)
find_package(Spdlog REQUIRED)

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
  set(CATTLESHED_LISTEN_PORT 50051)
endif()

if(NOT CATTLESHED_SERVICE_ARGS)
  set(CATTLESHED_SERVICE_ARGS "-c /opt/wandbox/cattleshed-conf/compilers.default")
endif()

# --- cattleshed.pb.cc などの生成

file(MAKE_DIRECTORY "${CMAKE_CURRENT_BINARY_DIR}/proto")
add_custom_command(
  OUTPUT
    "${CMAKE_CURRENT_BINARY_DIR}/proto/cattleshed.pb.cc"
    "${CMAKE_CURRENT_BINARY_DIR}/proto/cattleshed.pb.h"
    "${CMAKE_CURRENT_BINARY_DIR}/proto/cattleshed.grpc.pb.cc"
    "${CMAKE_CURRENT_BINARY_DIR}/proto/cattleshed.grpc.pb.h"
  COMMAND $<TARGET_FILE:protobuf::protoc>
  ARGS
    --grpc_out "${CMAKE_CURRENT_BINARY_DIR}/proto"
    --cpp_out "${CMAKE_CURRENT_BINARY_DIR}/proto"
    -I "${CMAKE_CURRENT_SOURCE_DIR}/../proto"
    --plugin=protoc-gen-grpc="$<TARGET_FILE:gRPC::grpc_cpp_plugin>"
    "${CMAKE_CURRENT_SOURCE_DIR}/../proto/cattleshed.proto"
  DEPENDS "${CMAKE_CURRENT_SOURCE_DIR}/../proto/cattleshed.proto")

set(CATTLESHED_PROTO
  "${CMAKE_CURRENT_BINARY_DIR}/proto/cattleshed.pb.cc"
  "${CMAKE_CURRENT_BINARY_DIR}/proto/cattleshed.grpc.pb.cc")

# --- 便利マクロ定義

macro(set_sanitizer target)
  # tsan
  if (ENABLE_TSAN)
    target_compile_options(${target} PRIVATE -g -fsanitize=thread -fsanitize=undefined)
    target_link_options(${target} PRIVATE -g -fsanitize=thread -fsanitize=undefined)
  endif()

  # asan
  if (ENABLE_ASAN)
    target_compile_options(${target} PRIVATE -g -fsanitize=address -fsanitize=undefined)
    target_link_options(${target} PRIVATE -g -fsanitize=address -fsanitize=undefined)
  endif()
endmacro()

# ---- cattleshed

add_executable(cattleshed
  src/server.cc
  src/load_config.cc
  src/quoted_printable.cc
  src/syslogstream.cc
  ${CATTLESHED_PROTO})
set_target_properties(cattleshed PROPERTIES CXX_STANDARD 14 C_STANDARD 99)
target_include_directories(cattleshed PRIVATE "${CMAKE_CURRENT_BINARY_DIR}/proto")
target_compile_definitions(cattleshed
  PRIVATE
    SYSCONFDIR="${CMAKE_INSTALL_PREFIX}/etc"
    SPDLOG_ACTIVE_LEVEL=SPDLOG_LEVEL_TRACE)
target_link_libraries(cattleshed
  Boost::boost
  Boost::program_options
  RT::RT
  Threads::Threads
  gRPC::grpc++
  Spdlog::Spdlog)

set_sanitizer(cattleshed)

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
