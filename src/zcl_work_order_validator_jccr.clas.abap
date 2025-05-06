CLASS zcl_work_order_validator_jccr DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.

    INTERFACES : if_oo_adt_classrun.

    METHODS:
      "
      read_order IMPORTING iv_id_work_order TYPE zde_work_order_id_jccr
                           out              TYPE REF TO if_oo_adt_classrun_out
                 RETURNING VALUE(rv_valid)  TYPE abap_bool,

      validate_create_order IMPORTING iv_id_customer   TYPE zde_costumer_id_jccr
                                      iv_id_technician TYPE zde_technician_id_jccr
                                      iv_priority      TYPE zde_priority_jccr
                                      out              TYPE REF TO if_oo_adt_classrun_out
                            RETURNING VALUE(rv_valid)  TYPE abap_bool,

      validate_update_order IMPORTING iv_id_work_order TYPE zde_work_order_id_jccr
                                      iv_status        TYPE zde_status_jccr
                                      out              TYPE REF TO if_oo_adt_classrun_out
                            RETURNING VALUE(rv_valid)  TYPE abap_bool,

      validate_delete_order IMPORTING iv_id_work_order TYPE zde_work_order_id_jccr
                                      iv_status        TYPE zde_status_jccr
                                      out              TYPE REF TO if_oo_adt_classrun_out
                            RETURNING VALUE(rv_valid)  TYPE abap_bool,

      validate_status_and_priority IMPORTING iv_status       TYPE zde_status_jccr
                                             iv_priority     TYPE zde_priority_jccr
                                             out             TYPE REF TO if_oo_adt_classrun_out
                                   RETURNING VALUE(rv_valid) TYPE abap_bool,
      constructor.

  PROTECTED SECTION.
  PRIVATE SECTION.

    CONSTANTS: c_valid_status   TYPE string VALUE 'PE CO',
               c_valid_priority TYPE string VALUE 'A'.

    DATA: go_crud TYPE REF TO zcl_work_order_crud_handlerjcc.

    METHODS:
      check_customer_exists IMPORTING iv_id_customer   TYPE zde_costumer_id_jccr
                                      out              TYPE REF TO if_oo_adt_classrun_out
                            RETURNING VALUE(rv_exists) TYPE abap_bool,
      check_technician_exists IMPORTING iv_id_technician TYPE zde_technician_id_jccr
                                        out              TYPE REF TO if_oo_adt_classrun_out
                              RETURNING VALUE(rv_exists) TYPE abap_bool,
      check_order_exists IMPORTING iv_id_work_order TYPE zde_work_order_id_jccr
                                   out              TYPE REF TO if_oo_adt_classrun_out
                         RETURNING VALUE(rv_exists) TYPE abap_bool,
      check_order_history IMPORTING iv_id_work_order TYPE zde_work_order_id_jccr
                                    out              TYPE REF TO if_oo_adt_classrun_out
                          RETURNING VALUE(rv_exists) TYPE abap_bool.

ENDCLASS.



CLASS zcl_work_order_validator_jccr IMPLEMENTATION.

  METHOD constructor.

*    IF go_crud IS INITIAL.
*      go_crud = NEW zcl_work_order_crud_handlerjcc( ).
*    ENDIF.

  ENDMETHOD.

  METHOD if_oo_adt_classrun~main.

*    DATA(lo_exec) = NEW zcl_work_order_validator_jccr( ).

    IF go_crud IS INITIAL.
      go_crud = NEW zcl_work_order_crud_handlerjcc( out ).
    ENDIF.

    DATA(lv_operacion) = 'CREARTECNICO'.
    .
    CASE lv_operacion.

      WHEN 'CREARCLIENTE'.
        TRY.
            INSERT ztcustomer_jcc FROM TABLE @(  VALUE #(  (  id_customer = 1
                                                                name = 'Juan Camilo Cadavid'
                                                                address = 'Calle 5 S N 8'
                                                                phone = 3126911427 ) ) ).
            IF  sy-subrc EQ 0.
              out->write( |Orden insertada correctamente. Registro: { sy-dbcnt }| ).
            ENDIF.
          CATCH cx_sy_open_sql_db INTO DATA(lx_error).
            out->write( |Error SQL: { lx_error->get_text( ) }| ).
            RETURN.
        ENDTRY.

      WHEN 'CREARTECNICO'.

        TRY.
            INSERT zttechnician_jcc FROM TABLE @(  VALUE #(  (  id_technicial = 001
                                                                name = 'Andres Gomez'
                                                                specialty = 'electronic engineer' ) ) ).
            IF  sy-subrc EQ 0.
              out->write( |Orden insertada correctamente. Registro: { sy-dbcnt }| ).
            ENDIF.
          CATCH cx_sy_open_sql_db INTO DATA(lx_errorr).
            out->write( |Error SQL: { lx_errorr->get_text( ) }| ).
            RETURN.
        ENDTRY.


      WHEN 'READ'.

        read_order( iv_id_work_order = '123456789'
                    out              = out ).

      WHEN 'CREATE'.
        "   Creacion orden de trabajo
*
        validate_create_order(
           iv_id_customer   = 'CUST999'
           iv_id_technician = 'TECH888'
           iv_priority      = 'B'
           out              = out
         ).

      WHEN 'UPDATE'.
        "   Actualizacion orden de trabajo
*    lo_exec->validate_delete_order(
*                        EXPORTING
*                          iv_id_work_order   = '0000000000'
*                          iv_status = 'C'
*                          out           = out ).
      WHEN 'DELETE'.
        "   Eliminacion orden de trabajo
*
*    lo_exec->validate_delete_order(
*                        EXPORTING
*                          iv_id_work_order   = '0000000000'
*                          iv_status = 'C'
*                          out           = out ).
      WHEN 'ESTADOYPRIORIDAD'.
        "   Estado y prioridad
*
*    lo_exec->validate_delete_order(
*                        EXPORTING
*                          iv_id_work_order   = '0000000000'
*                          iv_status = 'C'
*                          out           = out ).
    ENDCASE.


  ENDMETHOD.

  METHOD validate_create_order.

    " Validación de parámetros

    " Check if customer exists
    IF iv_id_customer   IS INITIAL OR
       iv_id_technician IS INITIAL OR
       iv_priority      IS INITIAL.
      "iv_priority NOT IN c_valid_priority.
      rv_valid = abap_false.
      RETURN.
    ENDIF.

    IF check_customer_exists( iv_id_customer = iv_id_customer
                              out            = out            ) = abap_false.

       out->write( 'Customer not exists' ).
       RETURN.

    ENDIF.

    IF check_technician_exists( iv_id_technician = iv_id_technician
                                out              = out            ) = abap_false.
      out->write( 'Technical not exists' ).
      RETURN.
    ENDIF.

    go_crud->create_work_order(
      EXPORTING
        iv_id_work_order = 123456789
        iv_id_customer   = iv_id_customer
        iv_id_technician = iv_id_technician
        iv_priority   = iv_priority ).

    rv_valid = abap_true.

  ENDMETHOD.

  METHOD read_order.

    go_crud->read_work_order(
      EXPORTING
        iv_id_work_order   = iv_id_work_order ).

  ENDMETHOD.

  METHOD validate_update_order.

    " Validación de parámetros
    IF iv_id_work_order IS INITIAL OR
       iv_status IS INITIAL.
      rv_valid = abap_false.
      RETURN.
    ENDIF.

    rv_valid = abap_true.

  ENDMETHOD.

  METHOD validate_delete_order.

    " Validación de parámetros
*    IF        iv_status IS INITIAL.
*      rv_valid = abap_false.
*      RETURN.
*    ENDIF.

    IF iv_id_work_order IS INITIAL OR
   iv_status IS INITIAL.
      rv_valid = abap_false.
      RETURN.
    ENDIF.

    Go_crud->delete_work_order(
      EXPORTING
        iv_id_work_order   = iv_id_work_order
        iv_status = iv_status
        out           = out ).

    rv_valid = abap_true.

  ENDMETHOD.

  METHOD validate_status_and_priority.

    IF iv_status IS INITIAL OR
       iv_priority IS INITIAL.
      rv_valid = abap_false.
      RETURN.
    ENDIF.

    rv_valid = abap_true.


  ENDMETHOD.

  METHOD check_customer_exists.

    CLEAR rv_exists.

    TRY.


        SELECT SINGLE id_customer,
                      name
          FROM ztcustomer_jcc
        WHERE id_customer EQ @iv_id_customer
        INTO @DATA(ls_customer).
        IF sy-subrc EQ 0.

          rv_exists = abap_true.

          out->write( name = 'work_order READ'
                      data = ls_customer   ).

        ENDIF.

      CATCH cx_sy_open_sql_db INTO DATA(lx_error).
        out->write( |Error SQL: { lx_error->get_text( ) }| ).
        RETURN.
    ENDTRY.

  ENDMETHOD.

  METHOD check_technician_exists.

    CLEAR rv_exists.

    TRY.

        SELECT SINGLE id_technicial,
                      name
          FROM zttechnician_jcc
        WHERE id_technicial EQ @iv_id_technician
        INTO @DATA(ls_technicial).
        IF sy-subrc EQ 0.

          rv_exists = abap_true.

          out->write( name = 'work_order READ'
                      data = ls_technicial ).

        ENDIF.

      CATCH cx_sy_open_sql_db INTO DATA(lx_error).
        out->write( |Error SQL: { lx_error->get_text( ) }| ).
        RETURN.
    ENDTRY.

  ENDMETHOD.

  METHOD check_order_exists.

  ENDMETHOD.

  METHOD check_order_history.

  ENDMETHOD.

ENDCLASS.
