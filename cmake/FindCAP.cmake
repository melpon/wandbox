find_path(CAP_INCLUDE_DIR NAMES sys/capability.h)
find_library(CAP_LIBRARY NAMES cap)

include(FindPackageHandleStandardArgs)
find_package_handle_standard_args(CAP DEFAULT_MSG CAP_LIBRARY CAP_INCLUDE_DIR)

mark_as_advanced(CAP_INCLUDE_DIR CAP_LIBRARY)

if(CAP_FOUND)
  if(NOT TARGET CAP::CAP)
    add_library(CAP::CAP UNKNOWN IMPORTED)
    set_target_properties(CAP::CAP PROPERTIES
      INTERFACE_INCLUDE_DIRECTORIES "${CAP_INCLUDE_DIR}"
      IMPORTED_LOCATION "${CAP_LIBRARY}")
  endif()
endif()

