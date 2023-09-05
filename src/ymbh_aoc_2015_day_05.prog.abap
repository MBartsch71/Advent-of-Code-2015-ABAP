REPORT ymbh_aoc_2015_day_05.



CLASS input_reader DEFINITION.
  PUBLIC SECTION.
    METHODS read_file_in_stringtab RETURNING VALUE(result) TYPE stringtab.

ENDCLASS.

CLASS input_reader IMPLEMENTATION.

  METHOD read_file_in_stringtab.
    DATA(file_reader) = NEW zcl_mbh_file_upload( |/Users/mbartsch71/github/Advent-of-Code-2015-ABAP/inputs/20151205| ).
    result = file_reader->file_upload_in_stringtab( ).
  ENDMETHOD.

ENDCLASS.

CLASS nice_string DEFINITION.
  PUBLIC SECTION.

    METHODS constructor IMPORTING content TYPE string.

    METHODS is_nice RETURNING VALUE(result) TYPE abap_bool.

    METHODS is_more_nice RETURNING VALUE(result) TYPE abap_bool.

  PRIVATE SECTION.
    DATA value TYPE REF TO ycl_mbh_string.

    METHODS string_has_vowels IMPORTING amount        TYPE i
                              RETURNING VALUE(result) TYPE abap_bool.

    METHODS string_has_twin_letters RETURNING VALUE(result) TYPE abap_bool.

    METHODS string_has_twin_letters_twice RETURNING VALUE(result) TYPE abap_bool.

    METHODS string_has_forbidden_pattern IMPORTING patterns      TYPE stringtab
                                         RETURNING VALUE(result) TYPE abap_bool.

ENDCLASS.

CLASS nice_string IMPLEMENTATION.

  METHOD constructor.
    value = ycl_mbh_string=>new( content ).
  ENDMETHOD.

  METHOD string_has_forbidden_pattern.
    DATA(iterator) = NEW ycl_mbh_stringtab_iterator( patterns ).
    WHILE iterator->has_next( ).
      DATA(sequence) = CAST ycl_mbh_string( iterator->get_next( ) ).
      IF value->contain_sequence( sequence->value( ) ).
        result = abap_true.
        RETURN.
      ENDIF.
    ENDWHILE.
  ENDMETHOD.

  METHOD string_has_twin_letters.
    result = value->has_connected_twins( ).
  ENDMETHOD.

  METHOD string_has_vowels.
    result = xsdbool( value->count_characters( ycl_mbh_string=>new( |aeiou| ) ) >= amount ).
  ENDMETHOD.

  METHOD is_nice.
    IF NOT string_has_vowels( amount = 3 ).
      RETURN.
    ENDIF.

    IF NOT string_has_twin_letters( ).
      RETURN.
    ENDIF.

    IF string_has_forbidden_pattern( VALUE #( ( |ab| ) ( |cd| ) ( |pq| ) ( |xy| ) ) ).
      RETURN.
    ENDIF.

    result = abap_true.
  ENDMETHOD.

  METHOD is_more_nice.
    IF NOT string_has_twin_letters_twice( ).
      RETURN.
    ENDIF.
    result = abap_true.
  ENDMETHOD.

  METHOD string_has_twin_letters_twice.
    result = value->has_connected_twins_twice( ).
  ENDMETHOD.

ENDCLASS.

CLASS application DEFINITION FINAL.

  PUBLIC SECTION.
    METHODS nice_strings IMPORTING input         TYPE stringtab
                         RETURNING VALUE(result) TYPE i.

  PRIVATE SECTION.
    METHODS is_nice_string IMPORTING string        TYPE any
                           RETURNING VALUE(result) TYPE abap_bool.

ENDCLASS.

CLASS application IMPLEMENTATION.

  METHOD nice_strings.
    result = REDUCE #( INIT sum = 0
                       FOR line IN input
                       NEXT sum = COND #( WHEN is_nice_string( line ) THEN sum + 1
                                          ELSE sum )  ).
  ENDMETHOD.

  METHOD is_nice_string.
    DATA(string_to_investigate) = NEW nice_string( string ).
    result = string_to_investigate->is_nice( ).
  ENDMETHOD.

ENDCLASS.

CLASS tc_nice_string DEFINITION FINAL FOR TESTING
  DURATION SHORT
  RISK LEVEL HARMLESS.

  PRIVATE SECTION.
    DATA cut TYPE REF TO nice_string.

    METHODS acceptance_test_nice_string   FOR TESTING.
    METHODS acceptance_test_no_nice_string FOR TESTING.
    METHODS accptnce_test_more_nice_string FOR TESTING.
ENDCLASS.

CLASS tc_nice_string IMPLEMENTATION.

  METHOD acceptance_test_nice_string.
    cut = NEW #( |ugknbfddgicrmopn| ).
    cl_abap_unit_assert=>assert_true( act = cut->is_nice( ) ).
  ENDMETHOD.

  METHOD acceptance_test_no_nice_string.
    cut = NEW #( |jchzalrnumimnmhp| ).
    cl_abap_unit_assert=>assert_false( act = cut->is_nice( ) ).
  ENDMETHOD.

  METHOD accptnce_test_more_nice_string.
    cut = NEW #( |uurcxstgmygtbstg| ).
    cl_abap_unit_assert=>assert_true( act = cut->is_more_nice( ) ).
  ENDMETHOD.

ENDCLASS.

CLASS tc_application DEFINITION FINAL FOR TESTING
  DURATION SHORT
  RISK LEVEL HARMLESS.

  PRIVATE SECTION.
    METHODS get_application_object FOR TESTING.
    METHODS get_two_nice_strings   FOR TESTING.
ENDCLASS.

CLASS tc_application IMPLEMENTATION.

  METHOD get_application_object.
    DATA(cut) = NEW application( ).
    cl_abap_unit_assert=>assert_bound( act = cut msg = |The object should be bound!| ).

  ENDMETHOD.

  METHOD get_two_nice_strings.
    DATA(cut) = NEW application( ).
    DATA(input) = VALUE stringtab( ( |ugknbfddgicrmopn| )
                                   ( |aaa| )
                                   ( |jchzalrnumimnmhp| )
                                   ( |haegwjzuvuyypxyu| )
                                   ( |dvszwmarrgswjxmb| ) ).

    cl_abap_unit_assert=>assert_equals( exp = 2 act = cut->nice_strings( input )  ).
  ENDMETHOD.

ENDCLASS.


START-OF-SELECTION.

  DATA(input) = NEW input_reader( )->read_file_in_stringtab( ).

  DATA(application) = NEW application( ).
  WRITE / |Result part one: { application->nice_strings( input ) }|.
