REPORT ymbh_aoc_2015_day_01.

CLASS input_reader DEFINITION.
  PUBLIC SECTION.
    METHODS read_file_in_string RETURNING VALUE(result) TYPE string.

ENDCLASS.

CLASS input_reader IMPLEMENTATION.

  METHOD read_file_in_string.
    DATA(file_reader) = NEW zcl_mbh_file_upload( |/Users/mbartsch71/github/Advent-of-Code-2015-ABAP/inputs/20151201| ).
    DATA(result_tab) = file_reader->file_upload_in_stringtab( ).
    result = result_tab[ 1 ].
  ENDMETHOD.

ENDCLASS.

CLASS movement DEFINITION.
  PUBLIC SECTION.
    METHODS get_floor RETURNING VALUE(result) TYPE i.

    METHODS get_position RETURNING VALUE(result) TYPE i.

    METHODS move IMPORTING instructions  TYPE string
                 RETURNING VALUE(result) TYPE i.

  PRIVATE SECTION.
    TYPES basetype TYPE c LENGTH 1.
    TYPES: BEGIN OF ENUM direction_type BASE TYPE basetype,
             nothing VALUE IS INITIAL,
             up      VALUE '(',
             down    VALUE ')',
           END OF ENUM direction_type.

    DATA current_floor TYPE i.
    DATA entering_basement_at_position TYPE i.
    DATA basement_visited TYPE abap_bool.

    METHODS basement_not_yet_visited RETURNING VALUE(result) TYPE abap_bool.

    METHODS increase_position.

    METHODS move_to_floor IMPORTING direction TYPE direction_type.

    METHODS entered_floor_is_basement RETURNING VALUE(result) TYPE abap_bool.

    METHODS mark_basement_visited.

    METHODS move_one_floor_up.

    METHODS move_one_floor_down.

    METHODS count_pos_till_basement_visit.

    METHODS mark_basement_entering_pos.

    METHODS make_instructions_iterable IMPORTING instructions  TYPE string
                                       RETURNING VALUE(result) TYPE REF TO yif_mbh_iterator.

    METHODS convert_to_enum IMPORTING iterator_object TYPE REF TO object
                           RETURNING VALUE(result)   TYPE direction_type.

ENDCLASS.

CLASS movement IMPLEMENTATION.

  METHOD get_floor.
    result = current_floor.
  ENDMETHOD.

  METHOD get_position.
    result = entering_basement_at_position.
  ENDMETHOD.

  METHOD move.
    DATA(iterator) = make_instructions_iterable( instructions ).
    WHILE iterator->has_next( ).
      count_pos_till_basement_visit( ).
      move_to_floor( convert_to_enum( iterator->get_next( ) ) ).
      mark_basement_entering_pos( ).
    ENDWHILE.
  ENDMETHOD.

  METHOD make_instructions_iterable.
    DATA(instructions_interable_string) = ycl_mbh_string=>new( instructions ).
    result = instructions_interable_string->get_iterator( ).
  ENDMETHOD.

  METHOD mark_basement_entering_pos.
    IF entered_floor_is_basement( ).
      mark_basement_visited( ).
    ENDIF.
  ENDMETHOD.

  METHOD count_pos_till_basement_visit.
    IF basement_not_yet_visited( ).
      increase_position( ).
    ENDIF.
  ENDMETHOD.

  METHOD basement_not_yet_visited.
    result = xsdbool( basement_visited = abap_false ).
  ENDMETHOD.

  METHOD increase_position.
    entering_basement_at_position = entering_basement_at_position + 1.
  ENDMETHOD.

  METHOD move_to_floor.
    IF direction = up.
      move_one_floor_up( ).
    ENDIF.

    IF direction = down.
      move_one_floor_down( ).
    ENDIF.
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

  METHOD convert_to_enum.
    DATA(string_object) = CAST ycl_mbh_string( iterator_object ).
    result = SWITCH #( string_object->value( ) WHEN '(' THEN up
                                               WHEN ')' THEN down ).
  ENDMETHOD.

ENDCLASS.


CLASS tc_floor_parser DEFINITION FINAL FOR TESTING
  DURATION SHORT
  RISK LEVEL HARMLESS.

  PRIVATE SECTION.
    DATA cut TYPE REF TO movement.

    METHODS assert_movement IMPORTING direction_pattern  TYPE string
                                      expected_end_floor TYPE i.

    METHODS assert_position IMPORTING direction_pattern TYPE string
                                      expected_position TYPE i.

    METHODS get_floors_after_instructions  FOR TESTING.
    METHODS get_postn_of_moving_in_basemnt FOR TESTING.

ENDCLASS.

CLASS tc_floor_parser IMPLEMENTATION.

  METHOD assert_movement.
    cut = NEW #( ).
    cut->move( direction_pattern ).
    cl_abap_unit_assert=>assert_equals( exp = expected_end_floor
                                        act = cut->get_floor( )
                                        msg = |Input: { direction_pattern }| ).
  ENDMETHOD.

  METHOD assert_position.
    cut = NEW #( ).
    cut->move( direction_pattern ).
    cl_abap_unit_assert=>assert_equals( exp = expected_position
                                        act = cut->get_position( )
                                        msg = |Input: { direction_pattern }| ).
  ENDMETHOD.

  METHOD get_floors_after_instructions.
    assert_movement( direction_pattern = |(())|    expected_end_floor = 0  ).
    assert_movement( direction_pattern = |()()|    expected_end_floor = 0  ).
    assert_movement( direction_pattern = |(()(()(| expected_end_floor = 3  ).
    assert_movement( direction_pattern = |(((|     expected_end_floor = 3  ).
    assert_movement( direction_pattern = |))(((((| expected_end_floor = 3  ).
    assert_movement( direction_pattern = |())|     expected_end_floor = -1  ).
    assert_movement( direction_pattern = |))(|     expected_end_floor = -1  ).
    assert_movement( direction_pattern = |)))|     expected_end_floor = -3  ).
    assert_movement( direction_pattern = |)())())| expected_end_floor = -3  ).
  ENDMETHOD.

  METHOD get_postn_of_moving_in_basemnt.
    assert_position( direction_pattern = |)|     expected_position = 1  ).
    assert_position( direction_pattern = |()())| expected_position = 5  ).
  ENDMETHOD.

ENDCLASS.

START-OF-SELECTION.

  DATA(movement) = NEW movement( ).
  movement->move( NEW input_reader( )->read_file_in_string( ) ).

  WRITE / |Result first part - floor { movement->get_floor( ) }|.
  WRITE / |Result second part - position: { movement->get_position( ) }|.
