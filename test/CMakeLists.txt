cmake_minimum_required(VERSION 3.15)

project(kennel C CXX)

set(CATTLESHED_LISTEN_PORT 2012)
set(CATTLESHED_BASEDIR ${CMAKE_CURRENT_SOURCE_DIR}/_tmp/wandbox)
set(CATTLESHED_STOREDIR ${CMAKE_CURRENT_SOURCE_DIR}/_tmp/log)
set(CATTLESHED_BINDIR ${CMAKE_CURRENT_BINARY_DIR}/cattleshed)

set(KENNEL_DATABASEDIR ${CMAKE_CURRENT_BINARY_DIR}/kennel2)
set(KENNEL_STATICDIR ${CMAKE_CURRENT_SOURCE_DIR}/static)
set(KENNEL_SPONSORSFILE ${CMAKE_CURRENT_SOURCE_DIR}/sponsors.json)
set(KENNEL_CATTLESHED_HOST "127.0.0.1")
set(KENNEL_CATTLESHED_PORT 2012)
set(KENNEL_SERVICE_API "http")

add_subdirectory(../cattleshed cattleshed)
add_subdirectory(../kennel2 kennel2)

find_package(Protobuf REQUIRED)
find_package(gRPC REQUIRED)
find_package(Spdlog REQUIRED)

enable_testing()

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

# ---- テスト本体のビルド

add_executable(test_grpc test_grpc.cpp ${CATTLESHED_PROTO})
set_target_properties(test_grpc PROPERTIES CXX_STANDARD 14 C_STANDARD 99)
target_compile_definitions(test_grpc PRIVATE SPDLOG_ACTIVE_LEVEL=SPDLOG_LEVEL_TRACE)
target_include_directories(test_grpc PRIVATE "${CMAKE_CURRENT_BINARY_DIR}/proto")
target_link_libraries(test_grpc
  gRPC::grpc++
  Spdlog::Spdlog)

set_sanitizer(test_grpc)

add_test(
  NAME test_grpc
  COMMAND $<TARGET_FILE:test_grpc>
)

add_test(
  NAME test_e2e
  COMMAND ${CMAKE_CURRENT_SOURCE_DIR}/test_e2e.sh
  WORKING_DIRECTORY ${CMAKE_CURRENT_SOURCE_DIR}
)
