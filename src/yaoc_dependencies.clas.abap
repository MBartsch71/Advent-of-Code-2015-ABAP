CLASS yaoc_dependencies DEFINITION
  PUBLIC
  CREATE PUBLIC .

  PUBLIC SECTION.
    INTERFACES if_apack_manifest.
    METHODS constructor.
ENDCLASS.



CLASS yaoc_dependencies IMPLEMENTATION.

  METHOD constructor.
    if_apack_manifest~descriptor = VALUE #( group_id = 'MBH_DEV'
                                             artifact_id = 'Advent-of-Code-2015-ABAP'
                                             version = '0.1'
                                             git_url = 'https://github.com/MBartsch71/Advent-of-Code-2015-ABAP.git'
                                             dependencies = VALUE #( ( group_id = 'MBH_DEV'
                                                                       artifact_id = 'abap-frontend-files'
                                                                       git_url = 'https://github.com/MBartsch71/abap-frontend-files.git' ) ) ).
  ENDMETHOD.
ENDCLASS.
