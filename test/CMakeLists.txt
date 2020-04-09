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

# ---- テストの設定

enable_testing()

if (ENABLE_TSAN)
  set(_E2E_ARGS --tsan)
endif()
if (ENABLE_ASAN)
  set(_E2E_ARGS --asan)
endif()

add_test(
  NAME test_e2e
  COMMAND ${CMAKE_CURRENT_SOURCE_DIR}/test_e2e.sh ${_E2E_ARGS}
  WORKING_DIRECTORY ${CMAKE_CURRENT_SOURCE_DIR}
)
