REPORT ymbh_aoc_2015_day_03.

INTERFACE coordinates.
  TYPES: BEGIN OF point,
           x TYPE i,
           y TYPE i,
         END OF point.
ENDINTERFACE.

CLASS input_reader DEFINITION.
  PUBLIC SECTION.
    METHODS read_file_in_stringtab RETURNING VALUE(result) TYPE stringtab.

ENDCLASS.

CLASS input_reader IMPLEMENTATION.

  METHOD read_file_in_stringtab.
    DATA(file_reader) = NEW zcl_mbh_file_upload( |/Users/mbartsch71/github/Advent-of-Code-2015-ABAP/inputs/20151203| ).
    result = file_reader->file_upload_in_stringtab( ).
  ENDMETHOD.

ENDCLASS.

CLASS santa DEFINITION FINAL.

  PUBLIC SECTION.
    METHODS position RETURNING VALUE(result) TYPE coordinates=>point.

    METHODS move IMPORTING direction TYPE char1.

  PRIVATE SECTION.
    DATA current_position TYPE coordinates=>point.

ENDCLASS.

CLASS santa IMPLEMENTATION.

  METHOD position.
    result = current_position.
  ENDMETHOD.

  METHOD move.
    current_position = VALUE #( BASE current_position
                                x = SWITCH #( direction WHEN '<' THEN current_position-x - 1
                                                        WHEN '>' THEN current_position-x + 1
                                                        ELSE current_position-x )
                                y = SWITCH #( direction WHEN '^' THEN current_position-y + 1
                                                        WHEN 'v' THEN current_position-y - 1
                                                        ELSE current_position-y ) ).
  ENDMETHOD.

ENDCLASS.

CLASS house DEFINITION FINAL.

  PUBLIC SECTION.
    METHODS constructor IMPORTING coordinates TYPE coordinates=>point.
    METHODS position RETURNING VALUE(result) TYPE coordinates=>point.

  PRIVATE SECTION.
    DATA house_position TYPE coordinates=>point.

ENDCLASS.

CLASS house IMPLEMENTATION.

  METHOD constructor.
    house_position = coordinates.
  ENDMETHOD.

  METHOD position.
    result = house_position.
  ENDMETHOD.

ENDCLASS.

CLASS house_collection DEFINITION FINAL.

  PUBLIC SECTION.
    METHODS size RETURNING VALUE(result) TYPE i.

    METHODS add IMPORTING house TYPE REF TO house.

  PRIVATE SECTION.
    DATA collection TYPE STANDARD TABLE OF REF TO house.

    METHODS house_exist IMPORTING house         TYPE REF TO house
                        RETURNING VALUE(result) TYPE abap_bool.

    METHODS add_house_to_collection IMPORTING house TYPE REF TO house.

ENDCLASS.

CLASS house_collection IMPLEMENTATION.

  METHOD size.
    result = lines( collection ).
  ENDMETHOD.

  METHOD add.
    IF NOT house_exist( house ).
      add_house_to_collection( house ).
    ENDIF.
  ENDMETHOD.

  METHOD house_exist.
    LOOP AT collection INTO DATA(saved_house).
      IF saved_house->position( ) = house->position( ).
        result = abap_true.
        RETURN.
      ENDIF.
    ENDLOOP.
  ENDMETHOD.

  METHOD add_house_to_collection.
    collection = VALUE #( BASE collection ( house ) ).
  ENDMETHOD.

ENDCLASS.

CLASS path_reader DEFINITION FINAL.

  PUBLIC SECTION.
    METHODS constructor IMPORTING steps TYPE stringtab.

    METHODS step RETURNING VALUE(result) TYPE char1.
    METHODS path RETURNING VALUE(result) TYPE REF TO ycl_mbh_string.

  PRIVATE SECTION.
    DATA path_layout TYPE REF TO ycl_mbh_string.
    DATA path_iterator TYPE REF TO ycl_mbh_string_iterator.

    METHODS merge_input IMPORTING input         TYPE stringtab
                        RETURNING VALUE(result) TYPE REF TO ycl_mbh_string.
ENDCLASS.

CLASS path_reader IMPLEMENTATION.

  METHOD constructor.
    path_layout = merge_input( steps ).
    path_iterator ?= path_layout->get_iterator( ).
  ENDMETHOD.

  METHOD merge_input.
    DATA(string) = REDUCE #( INIT res = ``
                       FOR line IN input
                       NEXT res = |{ res }{ line }| ).
    result = ycl_mbh_string=>new( string ).
  ENDMETHOD.

  METHOD step.
    DATA step TYPE REF TO ycl_mbh_string.
    IF path_iterator->has_next( ).
      step ?= path_iterator->get_next( ).
      result = step->value( ).
    ENDIF.
  ENDMETHOD.

  METHOD path.
    result = path_layout.
  ENDMETHOD.

ENDCLASS.

CLASS application DEFINITION FINAL.

  PUBLIC SECTION.
    METHODS main IMPORTING path_description TYPE stringtab
                 RETURNING VALUE(result)    TYPE i.

    METHODS main2 IMPORTING input         TYPE stringtab
                  RETURNING VALUE(result) TYPE i.

ENDCLASS.

CLASS application IMPLEMENTATION.

  METHOD main.
    DATA(path_reader) = NEW path_reader( path_description ).
    DATA(houses) = NEW house_collection( ).
    DATA(santa) = NEW santa( ).
    houses->add( NEW house( santa->position( ) ) ).
    DO.
      DATA(step) = path_reader->step( ).
      IF step IS INITIAL.
        EXIT.
      ENDIF.
      santa->move( step ).
      houses->add( NEW house( santa->position( ) ) ).
    ENDDO.
    result = houses->size( ).
  ENDMETHOD.


  METHOD main2.
    DATA step TYPE char1.

    DATA(path_reader) = NEW path_reader( input ).
    DATA(houses) = NEW house_collection( ).
    DATA(santa) = NEW santa( ).
    houses->add( NEW house( santa->position( ) ) ).
    DATA(robot_santa) = NEW santa( ).
    DO.
      step = path_reader->step( ).
      IF step IS INITIAL.
        EXIT.
      ENDIF.
      santa->move( step ).
      houses->add( NEW house( santa->position( ) ) ).
      step = path_reader->step( ).
      IF step IS INITIAL.
        EXIT.
      ENDIF.
      robot_santa->move( step ).
      houses->add( NEW house( robot_santa->position( ) ) ).
    ENDDO.
    result = houses->size( ).
  ENDMETHOD.

ENDCLASS.

CLASS tc_santa DEFINITION FINAL FOR TESTING
  DURATION SHORT
  RISK LEVEL HARMLESS.

  PRIVATE SECTION.
    DATA cut TYPE REF TO santa.

    METHODS setup.

    METHODS santa_has_starting_position   FOR TESTING.
    METHODS move_santa_one_pos_north      FOR TESTING.
    METHODS move_santa_one_step_south     FOR TESTING.
    METHODS move_santa_one_north_one_east FOR TESTING.
    METHODS move_santa_one_south_one_west FOR TESTING.

ENDCLASS.

CLASS tc_santa IMPLEMENTATION.

  METHOD setup.
    cut = NEW #( ).
  ENDMETHOD.

  METHOD santa_has_starting_position.
    DATA(expected_values) = VALUE coordinates=>point( x = 0 y = 0 ).
    cl_abap_unit_assert=>assert_equals( exp = expected_values act = cut->position( ) ).
  ENDMETHOD.

  METHOD move_santa_one_pos_north.
    cut->move( |^| ).
    DATA(expected_values) = VALUE coordinates=>point( x = 0 y = 1 ).
    cl_abap_unit_assert=>assert_equals( exp = expected_values act = cut->position( ) ).
  ENDMETHOD.

  METHOD move_santa_one_step_south.
    cut->move( |v| ).
    DATA(expected_values) = VALUE coordinates=>point( x = 0 y = -1 ).
    cl_abap_unit_assert=>assert_equals( exp = expected_values act = cut->position( ) ).
  ENDMETHOD.

  METHOD move_santa_one_north_one_east.
    cut->move( |^| ).
    cut->move( |<| ).
    DATA(expected_values) = VALUE coordinates=>point( x = -1 y = 1 ).
    cl_abap_unit_assert=>assert_equals( exp = expected_values act = cut->position( ) ).
  ENDMETHOD.

  METHOD move_santa_one_south_one_west.
    cut->move( |v| ).
    cut->move( |>| ).
    DATA(expected_values) = VALUE coordinates=>point( x = 1 y = -1 ).
    cl_abap_unit_assert=>assert_equals( exp = expected_values act = cut->position( ) ).
  ENDMETHOD.

ENDCLASS.

CLASS tc_house DEFINITION FINAL FOR TESTING
  DURATION SHORT
  RISK LEVEL HARMLESS.

  PRIVATE SECTION.
    METHODS build_house_at_coordinates FOR TESTING.

ENDCLASS.

CLASS tc_house IMPLEMENTATION.

  METHOD build_house_at_coordinates.
    DATA(cut) = NEW house( VALUE coordinates=>point( x = 1 y = 1 ) ).
    DATA(expected_values) = VALUE coordinates=>point( x = 1 y = 1 ).
    cl_abap_unit_assert=>assert_equals( exp = expected_values act = cut->position( ) ).
  ENDMETHOD.

ENDCLASS.

CLASS tc_house_collection DEFINITION FINAL FOR TESTING
  DURATION SHORT
  RISK LEVEL HARMLESS.

  PRIVATE SECTION.
    DATA cut TYPE REF TO house_collection.

    METHODS setup.

    METHODS add_a_house_to_the_collection FOR TESTING.
    METHODS add_two_houses_to_collection  FOR TESTING.
    METHODS dont_add_house_if_exists      FOR TESTING.

ENDCLASS.

CLASS tc_house_collection IMPLEMENTATION.

  METHOD setup.
    cut = NEW #( ).
  ENDMETHOD.

  METHOD add_a_house_to_the_collection.
    cut->add( NEW house( VALUE #( x = 0 y = 0 ) ) ).
    cl_abap_unit_assert=>assert_equals( exp = 1 act = cut->size( ) ).
  ENDMETHOD.

  METHOD add_two_houses_to_collection.
    cut->add( NEW house( VALUE #( x = 0 y = 0 ) ) ).
    cut->add( NEW house( VALUE #( x = 1 y = 0 ) ) ).
    cl_abap_unit_assert=>assert_equals( exp = 2 act = cut->size( ) ).
  ENDMETHOD.

  METHOD dont_add_house_if_exists.
    cut->add( NEW house( VALUE #( x = 0 y = 0 ) ) ).
    cut->add( NEW house( VALUE #( x = 0 y = 0 ) ) ).
    cl_abap_unit_assert=>assert_equals( exp = 1 act = cut->size( ) ).
  ENDMETHOD.

ENDCLASS.

CLASS tc_path_reader DEFINITION FINAL FOR TESTING
  DURATION SHORT
  RISK LEVEL HARMLESS.

  PRIVATE SECTION.
    DATA cut TYPE REF TO path_reader.

    METHODS setup.
    METHODS get_string_from_input         FOR TESTING.
    METHODS get_first_step_from_path      FOR TESTING.
    METHODS get_full_string_from_iterator FOR TESTING.
ENDCLASS.

CLASS tc_path_reader IMPLEMENTATION.

  METHOD setup.
    cut = NEW #( VALUE #( ( |^>v| ) ) ).
  ENDMETHOD.

  METHOD get_string_from_input.
    cl_abap_unit_assert=>assert_equals( exp = |^>v|  act = cut->path( )->value( )  ).
  ENDMETHOD.

  METHOD get_first_step_from_path.
    cl_abap_unit_assert=>assert_equals( exp = |^| act = cut->step( )  ).
  ENDMETHOD.

  METHOD get_full_string_from_iterator.
    DATA iterated_string TYPE string.
    DO.
      DATA(letter) = cut->step( ).
      IF letter IS INITIAL.
        EXIT.
      ENDIF.
      iterated_string = |{ iterated_string }{ letter }|.
    ENDDO.
    cl_abap_unit_assert=>assert_equals( exp = |^>v| act = iterated_string  ).
  ENDMETHOD.

ENDCLASS.

CLASS tc_application DEFINITION FINAL FOR TESTING
  DURATION SHORT
  RISK LEVEL HARMLESS.

  PRIVATE SECTION.
    DATA cut TYPE REF TO application.

    METHODS setup.
    METHODS visit_two_houses_one_step     FOR TESTING.
    METHODS visit_four_houses_four_steps  FOR TESTING.
    METHODS visit_two_house_ten_steps     FOR TESTING.

    METHODS two_santas_three_houses       FOR TESTING.
    METHODS two_santas_three_houses_again FOR TESTING.
    METHODS two_santas_11_houses          FOR TESTING.
ENDCLASS.

CLASS tc_application IMPLEMENTATION.

  METHOD setup.
    cut = NEW #( ).
  ENDMETHOD.

  METHOD visit_two_houses_one_step.
    cl_abap_unit_assert=>assert_equals( exp = 2 act = cut->main( VALUE #( ( |>| ) ) ) ).
  ENDMETHOD.

  METHOD visit_four_houses_four_steps.
    cl_abap_unit_assert=>assert_equals( exp = 4 act = cut->main( VALUE #( ( |^>v<| ) ) ) ).
  ENDMETHOD.

  METHOD visit_two_house_ten_steps.
    cl_abap_unit_assert=>assert_equals( exp = 2 act = cut->main( VALUE #( ( |^v^v^v^v^v| ) ) ) ).
  ENDMETHOD.

  METHOD two_santas_three_houses.
    cl_abap_unit_assert=>assert_equals( exp = 3 act = cut->main2( VALUE #( ( |^v| ) ) ) ).
  ENDMETHOD.

  METHOD two_santas_three_houses_again.
    cl_abap_unit_assert=>assert_equals( exp = 3 act = cut->main2( VALUE #( ( |^>v<| ) ) ) ).
  ENDMETHOD.

  METHOD two_santas_11_houses.
    cl_abap_unit_assert=>assert_equals( exp = 11 act = cut->main2( VALUE #( ( |^v^v^v^v^v| ) ) ) ).
  ENDMETHOD.

ENDCLASS.

START-OF-SELECTION.

  DATA(input) = NEW input_reader( )->read_file_in_stringtab( ).
  DATA(application) = NEW application( ).

  WRITE / |Solution part 1: { application->main( input ) }|.
  WRITE / |Solution part 2: { application->main2( input ) }|.
