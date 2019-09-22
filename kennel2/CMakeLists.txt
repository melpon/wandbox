cmake_minimum_required(VERSION 3.5)

project(kennel C CXX)

find_package(OpenSSL REQUIRED)
find_package(SQLite3 REQUIRED)
find_package(CURL REQUIRED)
find_package(ZLIB REQUIRED)
find_package(CppCMS REQUIRED)
find_package(CppDB REQUIRED)
find_package(Threads REQUIRED)

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

# set(KENNEL_JSON ${CMAKE_CURRENT_BINARY_DIR}/kennel.json)
# set(_KENNEL_JSON_IN ${PROJECT_SOURCE_DIR}/kennel.json.in)
# 
# configure_file(${_KENNEL_JSON_IN} ${KENNEL_JSON} @ONLY

# ---- kennel 本体のビルド

add_executable(kennel src/kennel.cpp ${SKIN_KENNEL})
set_target_properties(kennel PROPERTIES CXX_STANDARD 14 C_STANDARD 99)
target_link_libraries(kennel
  CppCMS::CppCMS
  CppDB::CppDB
  CURL::libcurl
  SQLite::SQLite3
  OpenSSL::Crypto
  OpenSSL::SSL
  ZLIB::ZLIB
  Threads::Threads)

# ---- インストール

# install(TARGETS kennel)
# install(FILES sponsors.json

