REPORT ymbh_aoc_2015_day_02.

CLASS input_reader DEFINITION.
  PUBLIC SECTION.
    METHODS read_file_in_stringtab RETURNING VALUE(result) TYPE stringtab.

ENDCLASS.

CLASS input_reader IMPLEMENTATION.

  METHOD read_file_in_stringtab.
    DATA(file_reader) = NEW zcl_mbh_file_upload( |/Users/mbartsch71/github/Advent-of-Code-2015-ABAP/inputs/20151202| ).
    result = file_reader->file_upload_in_stringtab( ).
  ENDMETHOD.

ENDCLASS.

CLASS paper_calculator DEFINITION.

  PUBLIC SECTION.
    METHODS constructor IMPORTING input TYPE string.

    METHODS calc RETURNING VALUE(result) TYPE i.

  PRIVATE SECTION.
    TYPES input_factors TYPE STANDARD TABLE OF i WITH EMPTY KEY.
    TYPES products TYPE SORTED TABLE OF i WITH NON-UNIQUE KEY primary_key COMPONENTS table_line.

    DATA factors TYPE input_factors.

    METHODS split_factors IMPORTING input         TYPE string
                          RETURNING VALUE(result) TYPE input_factors.

    METHODS calc_3_sides IMPORTING input         TYPE input_factors
                         RETURNING VALUE(result) TYPE products.

    METHODS calc_line IMPORTING line          TYPE i
                                input         TYPE input_factors
                      RETURNING VALUE(result) TYPE products.

    METHODS double_product_sides IMPORTING input         TYPE products
                                 RETURNING VALUE(result) TYPE products.

    METHODS cal_index IMPORTING index         TYPE i
                      RETURNING VALUE(result) TYPE i.

    METHODS add_smallest_side IMPORTING input         TYPE products
                              RETURNING VALUE(result) TYPE products.

    METHODS calculate_square_feet IMPORTING input         TYPE products
                                  RETURNING VALUE(result) TYPE i.

ENDCLASS.

CLASS paper_calculator IMPLEMENTATION.

  METHOD constructor.
    factors = split_factors( input ).
  ENDMETHOD.

  METHOD calc.
    DATA(single_side_products) = calc_3_sides( factors ).
    DATA(double_side_products) = double_product_sides( single_side_products ).
    DATA(sides_plus_reserve) = add_smallest_side( double_side_products ).
    result = calculate_square_feet( sides_plus_reserve ).
  ENDMETHOD.

  METHOD split_factors.
    SPLIT input AT 'x' INTO TABLE DATA(string_factors).
    result = string_factors.
  ENDMETHOD.

  METHOD calc_3_sides.
    result = calc_line( line = 1 input = input ).
  ENDMETHOD.

  METHOD calc_line.
    IF line < lines( input ).
      result = calc_line( line = line + 1 input = input ).
    ENDIF.

    result = VALUE #( BASE result
                      FOR i = line THEN i + 1 WHILE i < lines( input )
                      LET index = cal_index( index )
                      IN
                      ( input[ line ] * input[ line + index ] )  ).
  ENDMETHOD.

  METHOD cal_index.
    result = index + 1.
  ENDMETHOD.

  METHOD add_smallest_side.
    INSERT input[ 1 ] INTO TABLE result.
    result = VALUE #( BASE result ( LINES OF input ) ).
  ENDMETHOD.

  METHOD calculate_square_feet.
    result = REDUCE #( INIT sum = 0
                           FOR line IN input
                           NEXT sum = sum + line ).
  ENDMETHOD.

  METHOD double_product_sides.
    result = VALUE #( FOR i = 1 THEN i + 1 WHILE i <= 2
                        ( LINES OF input ) ).
  ENDMETHOD.

ENDCLASS.

CLASS ribbon_calculator DEFINITION FINAL.

  PUBLIC SECTION.
    TYPES factors TYPE SORTED TABLE OF i WITH NON-UNIQUE KEY primary_key COMPONENTS table_line.

    METHODS constructor IMPORTING calculation TYPE string.

    METHODS ribbon RETURNING VALUE(result) TYPE i.

  PRIVATE SECTION.
    DATA calculation_factors TYPE factors.

    METHODS split_expressions_into_factors IMPORTING i_calculation TYPE string.

    METHODS double_calculation_factor IMPORTING factor        TYPE i
                                      RETURNING VALUE(result) TYPE i.

    METHODS calculate_ribbon RETURNING VALUE(result) TYPE i.

    METHODS calculate_bow RETURNING VALUE(result) TYPE i.

ENDCLASS.

CLASS ribbon_calculator IMPLEMENTATION.

  METHOD constructor.
    split_expressions_into_factors( calculation ).
  ENDMETHOD.

  METHOD ribbon.
    result = calculate_ribbon( ) + calculate_bow( ).
  ENDMETHOD.

  METHOD split_expressions_into_factors.
    SPLIT i_calculation AT 'x' INTO TABLE DATA(factors).
    calculation_factors = factors.
  ENDMETHOD.

  METHOD calculate_ribbon.
    result = REDUCE #( INIT sum = 0
                       FOR i = 1 THEN i + 1 WHILE i < lines( calculation_factors )
                       NEXT sum = sum + double_calculation_factor( calculation_factors[ i ] )  ).
  ENDMETHOD.

  METHOD calculate_bow.
    result = REDUCE #( INIT product = 1
                       FOR factor IN calculation_factors
                       NEXT product = product * factor ).
  ENDMETHOD.

  METHOD double_calculation_factor.
    result = factor * 2.
  ENDMETHOD.

ENDCLASS.

CLASS application DEFINITION FINAL.

  PUBLIC SECTION.
    METHODS constructor IMPORTING input TYPE stringtab.

    METHODS calculate_sqaure_feets RETURNING VALUE(result) TYPE i.

    METHODS calculate_ribbon RETURNING VALUE(result) TYPE i.

  PRIVATE SECTION.
    DATA terms TYPE stringtab.

    METHODS calculate_surface_from_term IMPORTING term          TYPE string
                                        RETURNING VALUE(result) TYPE i.

    METHODS calculate_ribbon_length IMPORTING term          TYPE string
                                    RETURNING VALUE(result) TYPE i.

ENDCLASS.

CLASS application IMPLEMENTATION.

  METHOD constructor.
    terms = input.
  ENDMETHOD.

  METHOD calculate_sqaure_feets.
    result = REDUCE #( INIT sum = 0
                       FOR term IN terms
                       NEXT sum = sum + calculate_surface_from_term( term ) ).
  ENDMETHOD.

  METHOD calculate_ribbon.
    result = REDUCE #( INIT sum = 0
                       FOR term IN terms
                       NEXT sum = sum + calculate_ribbon_length( term ) ).
  ENDMETHOD.

  METHOD calculate_surface_from_term.
    DATA(line_calculator) = NEW paper_calculator( term ).
    result = line_calculator->calc( ).
  ENDMETHOD.

  METHOD calculate_ribbon_length.
    DATA(ribbon_calculator) = NEW ribbon_calculator( term ).
    result = ribbon_calculator->ribbon( ).
  ENDMETHOD.

ENDCLASS.

CLASS tc_paper_calculator DEFINITION FINAL FOR TESTING
  DURATION SHORT
  RISK LEVEL HARMLESS.

  PRIVATE SECTION.
    TYPES input_factors TYPE STANDARD TABLE OF i WITH EMPTY KEY.

    DATA cut TYPE REF TO paper_calculator.

    METHODS get_product_from_input_term FOR TESTING.

ENDCLASS.

CLASS tc_paper_calculator IMPLEMENTATION.

  METHOD get_product_from_input_term.
    cut = NEW #( |2x3x4| ).
    cl_abap_unit_assert=>assert_equals( exp = 58 act = cut->calc( ) ).
  ENDMETHOD.

ENDCLASS.

CLASS tc_ribbon_calculator DEFINITION FINAL FOR TESTING
  DURATION SHORT
  RISK LEVEL HARMLESS.

  PRIVATE SECTION.
    TYPES factors TYPE SORTED TABLE OF i WITH NON-UNIQUE KEY primary_key COMPONENTS table_line.
    DATA cut TYPE REF TO ribbon_calculator.

    METHODS setup.

    METHODS get_length_for_ribbon FOR TESTING.
    METHODS get_length_for_secon_ribbon FOR TESTING.

ENDCLASS.

CLASS tc_ribbon_calculator IMPLEMENTATION.

  METHOD setup.
    cut = NEW #( |4x3x2| ).
  ENDMETHOD.

  METHOD get_length_for_ribbon.
    cl_abap_unit_assert=>assert_equals( exp = 34 act = cut->ribbon( ) ).
  ENDMETHOD.

  METHOD get_length_for_secon_ribbon.
    cut = NEW #( |1x1x10| ).
    cl_abap_unit_assert=>assert_equals( exp = 14  act = cut->ribbon( ) ).
  ENDMETHOD.

ENDCLASS.

CLASS tc_application DEFINITION FINAL FOR TESTING
  DURATION SHORT
  RISK LEVEL HARMLESS.

  PRIVATE SECTION.
    DATA cut TYPE REF TO application.

    METHODS setup.
    METHODS calculate_two_terms_paper FOR TESTING.
    METHODS calculate_two_terms_ribbon FOR TESTING.
ENDCLASS.

CLASS tc_application IMPLEMENTATION.

  METHOD setup.
    DATA(input) = VALUE stringtab( ( |2x3x4| )
                                   ( |1x1x10| ) ).
    cut = NEW #( input ).
  ENDMETHOD.

  METHOD calculate_two_terms_paper.
    cl_abap_unit_assert=>assert_equals( exp = 101 act = cut->calculate_sqaure_feets( )  ).
  ENDMETHOD.

  METHOD calculate_two_terms_ribbon.
    cl_abap_unit_assert=>assert_equals( exp = 48 act = cut->calculate_ribbon( ) ).
  ENDMETHOD.

ENDCLASS.

START-OF-SELECTION.

  DATA(input) = NEW input_reader( )->read_file_in_stringtab( ).
  DATA(application) = NEW application( input ).

  WRITE / |Solution part one: { application->calculate_sqaure_feets( ) }|.
  WRITE / |Solution part two: { application->calculate_ribbon( ) }|.
