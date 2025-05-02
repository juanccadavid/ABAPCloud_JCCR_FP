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

  protected SECTION.
  PRIVATE SECTION.

  CONSTANTS: c_valid_status TYPE string VALUE 'PE CO',
             c_valid_priority TYPE string VALUE 'A'.

  DATA: lo_crud TYPE REF TO zcl_work_order_crud_handlerjcc.

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
    IF lo_crud IS INITIAL.
      lo_crud = NEW zcl_work_order_crud_handlerjcc( ).
    ENDIF.
  ENDMETHOD.

METHOD if_oo_adt_classrun~main.

    DATA(lo_exec) = NEW zcl_work_order_validator_jccr( ).
*   Lectura orden de trabajo

    lo_exec->read_order(
                        EXPORTING
                          iv_id_work_order   = '0000000000'
                          out           = out ).

**   Creacion orden de trabajo
*
*    lo_exec->validate_create_order(
*                        EXPORTING
*                          iv_id_customer   = 'CUST999'
*                          iv_id_technician = 'TECH888'
*                          iv_priority   = 'B'
*                          out           = out ).

**   Actualizacion orden de trabajo
*    lo_exec->validate_delete_order(
*                        EXPORTING
*                          iv_id_work_order   = '0000000000'
*                          iv_status = 'C'
*                          out           = out ).

**   Eliminacion orden de trabajo
*
*    lo_exec->validate_delete_order(
*                        EXPORTING
*                          iv_id_work_order   = '0000000000'
*                          iv_status = 'C'
*                          out           = out ).

**   Estado y prioridad
*
*    lo_exec->validate_delete_order(
*                        EXPORTING
*                          iv_id_work_order   = '0000000000'
*                          iv_status = 'C'
*                          out           = out ).

ENDMETHOD.

METHOD validate_create_order.

    " Validación de parámetros
    IF iv_id_customer IS INITIAL OR
       iv_id_technician IS INITIAL OR
       iv_priority IS INITIAL.
       "iv_priority NOT IN c_valid_priority.
      rv_valid = abap_false.
      RETURN.
    ENDIF.

    lo_crud->create_work_order(
      EXPORTING
        iv_id_customer   = iv_id_customer
        iv_id_technician = iv_id_technician
        iv_priority   = iv_priority
        out           = out ).

    rv_valid = abap_true.

ENDMETHOD.

METHOD read_order.

  lo_crud->read_work_order(
    EXPORTING
      iv_id_work_order   = iv_id_work_order
      out           = out ).

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

    lo_crud->delete_work_order(
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

ENDMETHOD.

METHOD check_technician_exists.

ENDMETHOD.

METHOD check_order_exists.

ENDMETHOD.

METHOD check_order_history.

ENDMETHOD.

ENDCLASS.
