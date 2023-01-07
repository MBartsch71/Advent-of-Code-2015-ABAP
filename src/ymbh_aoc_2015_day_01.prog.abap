REPORT ymbh_aoc_2015_day_01.

CLASS input_reader DEFINITION.
  PUBLIC SECTION.
    METHODS read_file_in_string RETURNING VALUE(result) TYPE REF TO ycl_mbh_string.
ENDCLASS.

CLASS input_reader IMPLEMENTATION.

  METHOD read_file_in_string.
    DATA(file_reader) = NEW zcl_mbh_file_upload( |/Users/mbartsch71/github/Advent-of-Code-2015-ABAP/inputs/20151201| ).
    DATA(result_tab) = file_reader->file_upload_in_stringtab( ).
    result = ycl_mbh_string=>new( result_tab[ 1 ] ).
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
    DATA basement_visited TYPE abap_bool.

    METHODS move_to_floor IMPORTING direction TYPE REF TO ycl_mbh_string.

    METHODS basement_not_yet_visited RETURNING VALUE(result) TYPE abap_bool.
    METHODS entered_floor_is_basement RETURNING VALUE(result) TYPE abap_bool.

    METHODS increase_position.
    METHODS mark_basement_visited.
    METHODS move_one_floor_up.
    METHODS move_one_floor_down.

ENDCLASS.

CLASS movement IMPLEMENTATION.

  METHOD get_floor.
    result = current_floor.
  ENDMETHOD.

  METHOD get_position.
    result = entering_basement_at_position.
  ENDMETHOD.

  METHOD move.
    DATA(iterator) = instructions->get_iterator( ).
    WHILE iterator->has_next( ).

      IF basement_not_yet_visited( ).
        increase_position( ).
      ENDIF.

      move_to_floor( CAST #( iterator->get_next( ) ) ).

      IF entered_floor_is_basement( ).
        mark_basement_visited( ).
      ENDIF.

    ENDWHILE.
  ENDMETHOD.

  METHOD move_to_floor.
    IF direction->value( ) = |(|.
      move_one_floor_up( ).
    ENDIF.

    IF direction->value( ) = |)|.
      move_one_floor_down( ).
    ENDIF.
  ENDMETHOD.

  METHOD basement_not_yet_visited.
    result = xsdbool( basement_visited = abap_false ).
  ENDMETHOD.

  METHOD increase_position.
    entering_basement_at_position = entering_basement_at_position + 1.
  ENDMETHOD.

  METHOD entered_floor_is_basement.
    result = xsdbool( current_floor < 0 ).
  ENDMETHOD.

  METHOD mark_basement_visited.
    basement_visited = abap_true.
  ENDMETHOD.

  METHOD move_one_floor_up.
    current_floor = current_floor + 1.
  ENDMETHOD.

  METHOD move_one_floor_down.
    current_floor = current_floor - 1.
  ENDMETHOD.

ENDCLASS.


CLASS tc_floor_parser DEFINITION FINAL FOR TESTING
  DURATION SHORT
  RISK LEVEL HARMLESS.

  PRIVATE SECTION.
    DATA cut TYPE REF TO movement.

    METHODS assert_movement IMPORTING input    TYPE REF TO ycl_mbh_string
                                      expected TYPE i.

    METHODS assert_position IMPORTING input    TYPE REF TO ycl_mbh_string
                                      expected TYPE i.

    METHODS get_floors_after_instructions  FOR TESTING.
    METHODS get_postn_of_moving_in_basemnt FOR TESTING.

ENDCLASS.

CLASS tc_floor_parser IMPLEMENTATION.

  METHOD assert_movement.
    cut = NEW #( ).
    cut->move( input ).
    cl_abap_unit_assert=>assert_equals( exp = expected act = cut->get_floor( ) msg = |Input: { input->value( ) }| ).
  ENDMETHOD.

  METHOD assert_position.
    cut = NEW #( ).
    cut->move( input ).
    cl_abap_unit_assert=>assert_equals( exp = expected act = cut->get_position( ) msg = |Input: { input->value( ) }| ).
  ENDMETHOD.

  METHOD get_floors_after_instructions.
    assert_movement( input = ycl_mbh_string=>new( |(())| )    expected = 0 ).
    assert_movement( input = ycl_mbh_string=>new( |()()| )    expected = 0 ).
    assert_movement( input = ycl_mbh_string=>new( |(()(()(| ) expected = 3 ).
    assert_movement( input = ycl_mbh_string=>new( |(((| )     expected = 3 ).
    assert_movement( input = ycl_mbh_string=>new( |))(((((| ) expected = 3 ).
    assert_movement( input = ycl_mbh_string=>new( |())| )     expected = -1 ).
    assert_movement( input = ycl_mbh_string=>new( |))(| )     expected = -1 ).
    assert_movement( input = ycl_mbh_string=>new( |)))| )     expected = -3 ).
    assert_movement( input = ycl_mbh_string=>new( |)())())| ) expected = -3 ).
  ENDMETHOD.

  METHOD get_postn_of_moving_in_basemnt.
    assert_position( input = ycl_mbh_string=>new( |)| )     expected = 1 ).
    assert_position( input = ycl_mbh_string=>new( |()())| ) expected = 5 ).
  ENDMETHOD.

ENDCLASS.

START-OF-SELECTION.

  DATA(movement) = NEW movement( ).
  movement->move( NEW input_reader( )->read_file_in_string( ) ).

  WRITE / |Result first part - floor { movement->get_floor( ) }|.
  WRITE / |Result second part - position: { movement->get_position( ) }|.