cmake_minimum_required(VERSION 3.15)

project(kennel C CXX)

set(Boost_USE_STATIC_LIBS ON)

find_package(Boost REQUIRED COMPONENTS json)
find_package(OpenSSL REQUIRED)
find_package(SQLite3 REQUIRED)
find_package(CppDB REQUIRED)
find_package(Threads REQUIRED)
find_package(Protobuf REQUIRED)
find_package(gRPC REQUIRED)
find_package(Spdlog REQUIRED)
find_package(CLI11 REQUIRED)
find_package(Ggrpc REQUIRED)

# ---- 初期値

if (NOT KENNEL_HOST)
  # nginx 経由なのでリリース時でも 127.0.0.1
  set(KENNEL_PORT "127.0.0.1")
endif()

if (NOT KENNEL_PORT)
  set(KENNEL_PORT 3500)
endif()

if (NOT KENNEL_CATTLESHED_HOST)
  set(KENNEL_CATTLESHED_HOST "127.0.0.1")
endif()

if (NOT KENNEL_CATTLESHED_PORT)
  set(KENNEL_CATTLESHED_PORT 50051)
endif()

if (NOT KENNEL_SPONSORSFILE)
  set(KENNEL_SPONSORSFILE ${CMAKE_INSTALL_PREFIX}/etc/sponsors.json)
endif()

if (NOT KENNEL_DATABASE)
  set(KENNEL_DATABASE "sqlite3:db=${CMAKE_INSTALL_PREFIX}/var/lib/kennel/kennel_production.sqlite;@pool_size=10")
endif()

if (NOT KENNEL_URL)
  set(KENNEL_URL "https://wandbox.org")
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

# --- gRPC インターフェース用のファイルの生成

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
  DEPENDS
    "${CMAKE_CURRENT_SOURCE_DIR}/../proto/cattleshed.proto"
)

set(CATTLESHED_PROTO
  "${CMAKE_CURRENT_BINARY_DIR}/proto/cattleshed.pb.cc"
  "${CMAKE_CURRENT_BINARY_DIR}/proto/cattleshed.grpc.pb.cc")

# --- JSON インターフェース用のファイルの生成

file(MAKE_DIRECTORY "${CMAKE_CURRENT_BINARY_DIR}/proto")
add_custom_command(
  OUTPUT
    "${CMAKE_CURRENT_BINARY_DIR}/proto/kennel.json.h"
  COMMAND $<TARGET_FILE:protobuf::protoc>
  ARGS
    --jsonif-cpp_out "${CMAKE_CURRENT_BINARY_DIR}/proto"
    -I "${CMAKE_CURRENT_SOURCE_DIR}/../_install/protoc-gen-jsonif/proto"
    -I "${CMAKE_CURRENT_SOURCE_DIR}/../proto"
    --plugin=protoc-gen-jsonif-cpp="${PROTOC_GEN_JSONIF_CPP}"
    "${CMAKE_CURRENT_SOURCE_DIR}/../proto/kennel.proto"
  DEPENDS
    "${CMAKE_CURRENT_SOURCE_DIR}/../proto/kennel.proto"
)

set(KENNEL_PROTO_DEPS
  "${CMAKE_CURRENT_BINARY_DIR}/proto/kennel.json.h")

# ---- 静的ファイルの生成

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

add_executable(kennel src/main.cpp ${CATTLESHED_PROTO} ${KENNEL_PROTO_DEPS})
set_target_properties(kennel PROPERTIES CXX_STANDARD 17 C_STANDARD 99)
target_compile_definitions(kennel PRIVATE SPDLOG_ACTIVE_LEVEL=SPDLOG_LEVEL_TRACE)
target_include_directories(kennel PRIVATE "${CMAKE_CURRENT_BINARY_DIR}/proto")
target_link_libraries(kennel
  Boost::json
  protobuf::libprotobuf
  gRPC::grpc++
  CppDB::CppDB
  SQLite::SQLite3
  Threads::Threads
  Spdlog::Spdlog
  CLI11::CLI11
  Ggrpc::Ggrpc)

set_sanitizer(kennel)

# ---- インストール

set(SPONSORS_JSON ${PROJECT_SOURCE_DIR}/sponsors.json)

install(TARGETS kennel)
install(FILES ${SPONSORS_JSON} ${KENNEL_SERVICE} DESTINATION etc)