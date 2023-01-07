REPORT ymbh_aoc_2015_day_01.

CLASS input_reader DEFINITION.
  PUBLIC SECTION.
    METHODS read_file_in_string RETURNING VALUE(result) TYPE REF TO ycl_mbh_string.
ENDCLASS.

CLASS input_reader IMPLEMENTATION.

  METHOD read_file_in_string.
    DATA(file_reader) = NEW zcl_mbh_file_upload( |/Users/mbartsch71/github/Advent-of-Code-2015-ABAP/inputs/20151201| ).
    DATA(upload_table) = file_reader->file_upload_in_stringtab( ).

    DATA(upload_string) = REDUCE #( INIT res_string = ``
                                    FOR line IN upload_table
                                    NEXT res_string = res_string && line ).
    result = ycl_mbh_string=>new( upload_string ).
  ENDMETHOD.

ENDCLASS.

CLASS movement DEFINITION.
  PUBLIC SECTION.
    METHODS get_floor RETURNING VALUE(result) TYPE i.

    METHODS move IMPORTING instructions  TYPE REF TO ycl_mbh_string
                 RETURNING VALUE(result) TYPE REF TO ycl_mbh_integer.

    METHODS get_position RETURNING VALUE(result) TYPE i.

  PRIVATE SECTION.
    DATA current_floor TYPE i.
    DATA entering_basement_at_position TYPE i.

    METHODS move_to_floor IMPORTING direction TYPE string.

ENDCLASS.

CLASS movement IMPLEMENTATION.

  METHOD get_floor.
    result = current_floor.
  ENDMETHOD.

  METHOD move.
    DATA(no_more_counting) = abap_false.
    DATA(iterator) = instructions->get_iterator( ).
    WHILE iterator->has_next( ).
      IF no_more_counting = abap_false.
        entering_basement_at_position = entering_basement_at_position + 1.
      ENDIF.
      move_to_floor( CAST ycl_mbh_string( iterator->get_next( ) )->value( ) ).
      IF current_floor < 0.
        no_more_counting = abap_true.
      ENDIF.
    ENDWHILE.
  ENDMETHOD.

  METHOD move_to_floor.
    IF direction = |(|.
      current_floor = current_floor + 1.
    ELSEIF direction = |)|.
      current_floor = current_floor - 1.
    ENDIF.
  ENDMETHOD.

  METHOD get_position.
    result = entering_basement_at_position.
  ENDMETHOD.

ENDCLASS.


CLASS tc_floor_parser DEFINITION FINAL FOR TESTING
  DURATION SHORT
  RISK LEVEL HARMLESS.

  PRIVATE SECTION.
    DATA cut TYPE REF TO movement.

    METHODS setup.
    METHODS assert_movement IMPORTING input    TYPE string
                                      expected TYPE i.

    METHODS get_floors_after_instructions FOR TESTING.

ENDCLASS.

CLASS tc_floor_parser IMPLEMENTATION.

  METHOD get_floors_after_instructions.
    assert_movement( input = |(())|    expected = 0 ).
    assert_movement( input = |()()|    expected = 0 ).
    assert_movement( input = |(()(()(| expected = 3 ).
    assert_movement( input = |(((|     expected = 3 ).
    assert_movement( input = |))(((((| expected = 3 ).
    assert_movement( input = |())|     expected = -1 ).
    assert_movement( input = |))(|     expected = -1 ).
    assert_movement( input = |)))|     expected = -3 ).
    assert_movement( input = |)())())| expected = -3 ).
  ENDMETHOD.

  METHOD assert_movement.
    DATA(input_string) = ycl_mbh_string=>new( input ).
    cut->move( input_string ).
    cl_abap_unit_assert=>assert_equals( exp = expected act = cut->get_floor( ) msg = |Input: { input }| ).
  ENDMETHOD.

  METHOD setup.
    cut = NEW #( ).
  ENDMETHOD.

ENDCLASS.

START-OF-SELECTION.

  DATA(input_reader) = NEW input_reader( ).

  DATA(movement) = NEW movement( ).
  movement->move( input_reader->read_file_in_string( ) ).

  WRITE / |Result first part - floor { movement->get_floor( ) }|.
  WRITE / |Result second part - position: { movement->get_position( ) }|.
