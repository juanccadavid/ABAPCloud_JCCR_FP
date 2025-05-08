CLASS zcl_work_order_crud_handlertes DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.

    INTERFACES : if_oo_adt_classrun.

  METHODS:
    "CRUD
    test_create_work_order IMPORTING iv_id_work_order TYPE zde_work_order_id_jccr
                                     iv_id_customer   TYPE zde_costumer_id_jccr
                                     iv_id_technician TYPE zde_technician_id_jccr
                                     iv_status        TYPE zde_status_jccr
                                     iv_priority      TYPE zde_priority_jccr
                                     iv_description   TYPE zde_description_jccr
                                     out              TYPE REF TO if_oo_adt_classrun_out,

    test_read_work_order IMPORTING iv_id_work_order TYPE zde_work_order_id_jccr
                                   out              TYPE REF TO if_oo_adt_classrun_out,

    test_update_work_order IMPORTING iv_id_work_order TYPE zde_work_order_id_jccr
                                     iv_id_customer   TYPE zde_costumer_id_jccr
                                     iv_id_technician TYPE zde_technician_id_jccr
                                     iv_status        TYPE zde_status_jccr
                                     iv_priority      TYPE zde_priority_jccr
                                     iv_description   TYPE zde_description_jccr
                                     out              TYPE REF TO if_oo_adt_classrun_out,

    test_delete_work_order IMPORTING iv_id_work_order TYPE zde_work_order_id_jccr
                                     out              TYPE REF TO if_oo_adt_classrun_out.


  PROTECTED SECTION.
  PRIVATE SECTION.
ENDCLASS.



CLASS zcl_work_order_crud_handlertes IMPLEMENTATION.

METHOD if_oo_adt_classrun~main.

    DATA: lv_id_work_order TYPE zde_work_order_id_jccr VALUE '0523456789',
          lv_id_customer   TYPE zde_costumer_id_jccr VALUE '2',
          lv_id_technician TYPE zde_technician_id_jccr VALUE '01',
          lv_status        TYPE zde_status_jccr VALUE 'PE',
          lv_priority      TYPE zde_priority_jccr VALUE 'A',
          lv_description   TYPE zde_description_jccr VALUE 'Actualizacion de orden',

          lv_name_client TYPE string VALUE 'Fep Cadavid',
          lv_address_client TYPE string VALUE 'Calle 5 S N 8',
          lv_phone_client TYPE char13 VALUE 3126911427,

          lv_name_technicial TYPE string VALUE 'Diana Restrepo',
          lv_name_specialty TYPE string VALUE 'Sales Specialist'.

 "OPERACION PRINCIPAL
    DATA(lv_operacion_test) = 'test_read_work_order'.

    CASE lv_operacion_test.

    WHEN 'test_create_work_order'.
    "test_create_work_order
     test_create_work_order(
                        iv_id_work_order = lv_id_work_order
                        iv_id_customer = lv_id_customer
                        iv_id_technician = lv_id_technician
                        iv_status = lv_status
                        iv_priority = lv_priority
                        iv_description = lv_description
                        out = out ).
   WHEN 'test_read_work_order'.
   "test_read_work_order
   test_read_work_order(
                        iv_id_work_order = lv_id_work_order
                        out = out  ).

  WHEN 'test_update_work_order'.
  "test_update_work_order
   test_update_work_order(
                        iv_id_work_order = lv_id_work_order
                        iv_id_customer = lv_id_customer
                        iv_id_technician = lv_id_technician
                        iv_status = lv_status
                        iv_priority = lv_priority
                        iv_description = lv_description
                        out = out ).
 WHEN 'test_delete_work_order'.
 "test_delete_work_order
 test_delete_work_order(
                        iv_id_work_order = lv_id_work_order
                        out = out  ).
ENDCASE.

ENDMETHOD.

METHOD test_create_work_order.

    TRY.
        INSERT ztwork_order_jcc FROM TABLE @(  VALUE #(  (  id_work_order = iv_id_work_order
                                                            id_customer = iv_id_customer
                                                            id_technicial = iv_id_technician
                                                            creation_date = cl_abap_context_info=>get_system_date(  )
                                                            status = iv_status
                                                            priority = iv_priority
                                                            description = iv_description ) ) ).
        IF  sy-subrc EQ 0.
          out->write( |Orden insertada correctamente. Registro: { sy-dbcnt }| ).
        ENDIF.
      CATCH cx_sy_open_sql_db INTO DATA(lx_error).
        out->write( |Error SQL: { lx_error->get_text( ) }| ).
        RETURN.
    ENDTRY.

ENDMETHOD.

METHOD test_read_work_order.

    TRY.

        SELECT * FROM ztwork_order_jcc
        WHERE id_work_order EQ @iv_id_work_order
        INTO @DATA(ls_work_order).
        ENDSELECT.

        IF  sy-subrc EQ 0.
          out->write( name = 'work_order READ' data = ls_work_order ).
        ENDIF.
      CATCH cx_sy_open_sql_db INTO DATA(lx_error).
        out->write( |Error SQL: { lx_error->get_text( ) }| ).
        RETURN.

    ENDTRY.

ENDMETHOD.

METHOD test_update_work_order.

    TRY.
        UPDATE ztwork_order_jcc FROM TABLE @(  VALUE #(  (  id_work_order = iv_id_work_order
                                                            id_customer   = iv_id_customer
                                                            id_technicial = iv_id_technician
                                                            creation_date = cl_abap_context_info=>get_system_date(  )
                                                            status = iv_status
                                                            priority = iv_priority
                                                            description = iv_description ) ) ).
        IF  sy-subrc EQ 0.
          out->write( |Orden actualizada correctamente. Registro: { sy-dbcnt }|  ).
        ENDIF.
      CATCH cx_sy_open_sql_db INTO DATA(lx_error).
        out->write( |Error SQL: { lx_error->get_text( ) }| ).
        RETURN.
    ENDTRY.


ENDMETHOD.

METHOD test_delete_work_order.

    TRY.
        DELETE ztwork_order_jcc FROM TABLE @(  VALUE #(  ( id_work_order = iv_id_work_order ) ) ).
        IF  sy-subrc EQ 0.
          out->write( |Orden eliminada correctamente. Registro: { sy-dbcnt }| ).
        ENDIF.
      CATCH cx_sy_open_sql_db INTO DATA(lx_error).
        out->write( |Error SQL: { lx_error->get_text( ) }| ).
        RETURN.
    ENDTRY.

ENDMETHOD.

ENDCLASS.
