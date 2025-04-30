CLASS zcl_exec_01_c345_pf DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.

  INTERFACES: if_oo_adt_classrun.

  METHODS:
    "
    validate_create_order IMPORTING io_out TYPE REF TO if_oo_adt_classrun_out,
    validate_update_order IMPORTING io_out TYPE REF TO if_oo_adt_classrun_out,
    validate_delete_order IMPORTING io_out TYPE REF TO if_oo_adt_classrun_out,
    validate_status_and_priority IMPORTING io_out TYPE REF TO if_oo_adt_classrun_out.

    protected SECTION.
  PRIVATE SECTION.
ENDCLASS.



CLASS zcl_exec_01_c345_pf IMPLEMENTATION.

METHOD if_oo_adt_classrun~main.

*   Creacion orden de trabajo
    validate_create_order( out ).

**   Actualizacion orden de trabajo
*    Validate_update_order( out ).

**   Eliminacion orden de trabajo
*    validate_delete_order( out ).

**   Estado y prioridad
*    validate_status_and_priority( out ).

ENDMETHOD.

METHOD validate_create_order.

*"Check if customer exists
*DATA(lv_customer_exists) = check_customer_exists( iv_customer_id ).
*IF lv_customer_exists IS INITIAL.
*rv_valid = abap_false.
*RETURN.
*ENDIF.
*
*" Check if technician exists
*DATA(lv_technician_exists) = check_technician_exists( iv_technician_id ).
*IF lv_technician_exists IS INITIAL.
*rv_valid = abap_false.
*RETURN.
*ENDIF.
*
*" Check if priority is valid
*IF iv_priority NOT IN c_valid_priority.
*rv_valid = abap_false.
*RETURN.
*ENDIF.
*
*rv_valid = abap_true.
*
*io_out->write( 'Add records' ).
ENDMETHOD.

METHOD validate_update_order.

ENDMETHOD.

METHOD validate_delete_order.

ENDMETHOD.

METHOD validate_status_and_priority.

ENDMETHOD.

ENDCLASS.
