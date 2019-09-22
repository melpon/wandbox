find_path(PCRE_INCLUDE_DIR NAMES pcre.h PATHS "${PCRE_ROOT_DIR}/include")
find_library(PCRE_LIBRARY NAMES pcre PATHS "${PCRE_ROOT_DIR}/lib")

include(FindPackageHandleStandardArgs)
find_package_handle_standard_args(PCRE DEFAULT_MSG PCRE_LIBRARY PCRE_INCLUDE_DIR)

mark_as_advanced(PCRE_INCLUDE_DIR PCRE_LIBRARY)

if(PCRE_FOUND)
  if(NOT TARGET PCRE::PCRE)
    add_library(PCRE::PCRE UNKNOWN IMPORTED)
    set_target_properties(PCRE::PCRE PROPERTIES
      INTERFACE_INCLUDE_DIRECTORIES "${PCRE_INCLUDE_DIR}"
      IMPORTED_LOCATION "${PCRE_LIBRARY}")
  endif()
endif()

