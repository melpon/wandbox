find_path(CppDB_INCLUDE_DIR NAMES cppdb/defs.h PATHS "${CppDB_ROOT_DIR}/include" NO_DEFAULT_PATH)
find_library(CppDB_LIBRARY NAMES libcppdb.a PATHS "${CppDB_ROOT_DIR}/lib" NO_DEFAULT_PATH)

include(FindPackageHandleStandardArgs)
find_package_handle_standard_args(CppDB DEFAULT_MSG CppDB_LIBRARY CppDB_INCLUDE_DIR)

mark_as_advanced(CppDB_INCLUDE_DIR CppDB_LIBRARY)

if(CppDB_FOUND)
  if(NOT TARGET CppDB::CppDB)
    add_library(CppDB::CppDB STATIC IMPORTED)
    set_target_properties(CppDB::CppDB PROPERTIES
      INTERFACE_INCLUDE_DIRECTORIES "${CppDB_INCLUDE_DIR}"
      IMPORTED_LOCATION "${CppDB_LIBRARY}")
  endif()
endif()

