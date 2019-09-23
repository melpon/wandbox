find_path(CppCMS_INCLUDE_DIR NAMES cppcms/cppcms_error.h PATHS "${CppCMS_ROOT_DIR}/include" NO_DEFAULT_PATH)
find_library(CppCMS_LIBRARY NAMES cppcms PATHS "${CppCMS_ROOT_DIR}/lib" NO_DEFAULT_PATH)
find_library(CppCMS_Booster_LIBRARY NAMES booster PATHS "${CppCMS_ROOT_DIR}/lib" NO_DEFAULT_PATH)
find_program(CppCMS_cppcms_tmpl_cc NAMES cppcms_tmpl_cc PATHS "${CppCMS_ROOT_DIR}/bin" NO_DEFAULT_PATH)
find_package(ICU REQUIRED COMPONENTS uc i18n data)
find_package(PCRE REQUIRED)

include(FindPackageHandleStandardArgs)
find_package_handle_standard_args(CppCMS DEFAULT_MSG CppCMS_LIBRARY CppCMS_INCLUDE_DIR CppCMS_Booster_LIBRARY CppCMS_cppcms_tmpl_cc)

mark_as_advanced(CppCMS_INCLUDE_DIR CppCMS_LIBRARY CppCMS_Booster_LIBRARY)

if(CppCMS_FOUND)
  if(NOT TARGET CppCMS::Booster)
    add_library(CppCMS::Booster STATIC IMPORTED)
    set_target_properties(CppCMS::Booster PROPERTIES
      INTERFACE_INCLUDE_DIRECTORIES "${CppCMS_INCLUDE_DIR}"
      IMPORTED_LOCATION "${CppCMS_Booster_LIBRARY}")
  endif()

  if(NOT TARGET CppCMS::CppCMS)
    add_library(CppCMS::CppCMS STATIC IMPORTED)
    set_target_properties(CppCMS::CppCMS PROPERTIES
      INTERFACE_INCLUDE_DIRECTORIES "${CppCMS_INCLUDE_DIR}"
      INTERFACE_LINK_LIBRARIES "CppCMS::Booster;PCRE::PCRE;ICU::i18n;ICU::uc;ICU::data"
      IMPORTED_LOCATION "${CppCMS_LIBRARY}")
  endif()

  if(NOT TARGET CppCMS::cppcms_tmpl_cc)
    add_executable(CppCMS::cppcms_tmpl_cc IMPORTED)
    set_target_properties(CppCMS::cppcms_tmpl_cc PROPERTIES
      IMPORTED_LOCATION "${CppCMS_cppcms_tmpl_cc}")
  endif()
endif()

