CLASS zcl_03_branch DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.

    INTERFACES if_oo_adt_classrun .
  PROTECTED SECTION.
  PRIVATE SECTION.
ENDCLASS.

CLASS ZCL_03_BRANCH IMPLEMENTATION.


  METHOD if_oo_adt_classrun~main.

* Declarations
**************************

    DATA number1 TYPE i.
    DATA number2 TYPE i.

    DATA result TYPE p LENGTH 8 DECIMALS 2.


* Input Values
**************************

    number1 = -8.
    number2 = 3.

* Calculation
**************************

*    DATA(result) = number1 / number2.

    result = number1 / number2.

    DATA(output) = |{ number1 } / { number2 } = { result }|.


* Output
**************************

*    out->write( output ).

    DATA op TYPE c LENGTH 1.
    number1 = 456.
    number2 = 0.
    op = '/'.

    DATA output2 TYPE string.

*    IF op = '*'.
*     result = number1 * number2.
*    ELSEIF op = '/'.
*      result = number1 / number2.
*    ELSEIF op = '+'.
*      result = number1 + number2.
*    ELSEIF op = '-'.
*      result = number1 - number2.
*    ELSE.
*      out->write( 'operator not supported1' ).
*    ENDIF.

    TRY.
      CASE op.
        WHEN '*'. result = number1 * number2.
        WHEN '/'. result = number1 / number2.
        WHEN '+'. result = number1 + number2.
        WHEN '-'. result = number1 - number2.
        WHEN OTHERS. out->write( 'operator not supported2' ).
      ENDCASE.

      output2 = |{ number1 } { op } { number2 } = { result }|.
    CATCH cx_sy_zerodivide.
      output2 = |Division by zero is not defined|.
    ENDTRY.

    out->write( output2 ).

  ENDMETHOD.



ENDCLASS.
