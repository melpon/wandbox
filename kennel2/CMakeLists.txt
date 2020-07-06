cmake_minimum_required(VERSION 3.15)

project(kennel C CXX)

find_package(OpenSSL REQUIRED)
find_package(SQLite3 REQUIRED)
find_package(CURL REQUIRED)
find_package(ZLIB REQUIRED)
find_package(CppCMS REQUIRED)
find_package(CppDB REQUIRED)
find_package(Threads REQUIRED)
find_package(Protobuf REQUIRED)
find_package(gRPC REQUIRED)
find_package(Spdlog REQUIRED)
find_package(CLI11 REQUIRED)
find_package(Ggrpc REQUIRED)

# ---- 初期値

if(NOT KENNEL_DATABASEDIR)
  set(KENNEL_DATABASEDIR ${CMAKE_INSTALL_PREFIX}/var/lib/kennel)
endif()

if(NOT KENNEL_STATICDIR)
  set(KENNEL_STATICDIR ${CMAKE_INSTALL_PREFIX}/etc/static)
endif()

if(NOT KENNEL_SCHEME)
  set(KENNEL_SCHEME "https")
endif()

if(NOT KENNEL_DOMAIN)
  set(KENNEL_DOMAIN "wandbox.org")
endif()

if(NOT KENNEL_GOOGLEANALYTICS)
  set(KENNEL_GOOGLEANALYTICS "")
endif()

if(NOT KENNEL_SPONSORSFILE)
  set(KENNEL_SPONSORSFILE ${CMAKE_INSTALL_PREFIX}/etc/sponsors.json)
endif()

if(NOT KENNEL_GITHUBCLIENT)
  set(KENNEL_GITHUBCLIENT "")
endif()

if(NOT KENNEL_CATTLESHED_HOST)
  set(KENNEL_CATTLESHED_HOST "153.121.40.132")
endif()

if(NOT KENNEL_CATTLESHED_PORT)
  set(KENNEL_CATTLESHED_PORT 50051)
endif()

if(NOT KENNEL_SERVICE_API)
  set(KENNEL_SERVICE_API "fastcgi")
endif()

if(NOT KENNEL_SERVICE_PORT)
  set(KENNEL_SERVICE_PORT 3500)
endif()

if(NOT KENNEL_IPLIMIT_DURATION)
  # 秒
  set(KENNEL_IPLIMIT_DURATION 300)
endif()

if(NOT KENNEL_IPLIMIT_SIZE)
  # bytes
  # 1 * 1024 * 1024 = 1MB = 1048576
  set(KENNEL_IPLIMIT_SIZE 1048576)
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

# ---- skin_kennel.cpp の生成

set(_SKIN_KENNEL_SOURCES
  ${PROJECT_SOURCE_DIR}/src/common.tmpl
  ${PROJECT_SOURCE_DIR}/src/root.tmpl
  ${PROJECT_SOURCE_DIR}/src/user.tmpl
  ${PROJECT_SOURCE_DIR}/src/nojs_root.tmpl
  ${PROJECT_SOURCE_DIR}/src/nojs_list.tmpl)

set(SKIN_KENNEL "${CMAKE_CURRENT_BINARY_DIR}/skin_kennel.cpp")

add_custom_command(
  OUTPUT
    ${SKIN_KENNEL}
  COMMAND
    $<TARGET_FILE:CppCMS::cppcms_tmpl_cc>
  ARGS
    ${_SKIN_KENNEL_SOURCES}
    -o ${SKIN_KENNEL}
  DEPENDS
    ${_SKIN_KENNEL_SOURCES})

# 自動生成されたソースにはインクルードパスを追加
set_source_files_properties(${SKIN_KENNEL} PROPERTIES
  INCLUDE_DIRECTORIES "${PROJECT_SOURCE_DIR}/src")

# ---- 静的ファイルの生成

set(KENNEL_JSON ${CMAKE_CURRENT_BINARY_DIR}/kennel.json)
set(_KENNEL_JSON_IN ${PROJECT_SOURCE_DIR}/kennel.json.in)
configure_file(${_KENNEL_JSON_IN} ${KENNEL_JSON} @ONLY)

set(KENNEL_SERVICE ${CMAKE_CURRENT_BINARY_DIR}/kennel.service)
set(_KENNEL_SERVICE_IN ${PROJECT_SOURCE_DIR}/kennel.service.in)
configure_file(${_KENNEL_SERVICE_IN} ${KENNEL_SERVICE} @ONLY)

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

# ---- kennel 本体のビルド

add_executable(kennel src/kennel.cpp ${SKIN_KENNEL} ${CATTLESHED_PROTO})
set_target_properties(kennel PROPERTIES CXX_STANDARD 14 C_STANDARD 99)
target_compile_definitions(kennel PRIVATE SPDLOG_ACTIVE_LEVEL=SPDLOG_LEVEL_TRACE)
target_include_directories(kennel PRIVATE "${CMAKE_CURRENT_BINARY_DIR}/proto")
target_link_libraries(kennel
  protobuf::libprotobuf
  gRPC::grpc++
  CppCMS::CppCMS
  CppDB::CppDB
  CURL::libcurl
  SQLite::SQLite3
  ZLIB::ZLIB
  Threads::Threads
  Spdlog::Spdlog
  CLI11::CLI11
  Ggrpc::Ggrpc)

set_sanitizer(kennel)

# ---- インストール

set(SPONSORS_JSON ${PROJECT_SOURCE_DIR}/sponsors.json)

install(TARGETS kennel)
install(FILES ${SPONSORS_JSON} ${KENNEL_JSON} ${KENNEL_SERVICE} DESTINATION etc)
install(DIRECTORY ${PROJECT_SOURCE_DIR}/static DESTINATION etc)
install(FILES ${PROJECT_SOURCE_DIR}/../submodules/ansi_up/ansi_up.js DESTINATION etc/static/js)
install(FILES ${PROJECT_SOURCE_DIR}/../submodules/jquery-cookie/src/jquery.cookie.js DESTINATION etc/static/js)
