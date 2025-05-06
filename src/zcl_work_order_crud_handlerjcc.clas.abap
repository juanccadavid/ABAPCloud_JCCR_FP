CLASS zcl_work_order_crud_handlerjcc DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.

    METHODS:
      constructor
        IMPORTING
          out TYPE REF TO if_oo_adt_classrun_out.

    INTERFACES : if_oo_adt_classrun.

    METHODS:
      "CRUD
      create_work_order IMPORTING iv_id_work_order TYPE zde_work_order_id_jccr
                                  iv_id_customer   TYPE zde_costumer_id_jccr
                                  iv_id_technician TYPE zde_technician_id_jccr
                                  iv_priority      TYPE zde_priority_jccr,


      read_work_order IMPORTING iv_id_work_order TYPE zde_work_order_id_jccr,

      update_work_order IMPORTING iv_id_work_order TYPE zde_work_order_id_jccr
                                  iv_id_technician TYPE zde_technician_id_jccr
                                  iv_status        TYPE zde_status_jccr
                                  iv_priority      TYPE zde_priority_jccr
                                  out              TYPE REF TO if_oo_adt_classrun_out,

      delete_work_order IMPORTING iv_id_work_order TYPE zde_work_order_id_jccr
                                  iv_status        TYPE zde_status_jccr
                                  out              TYPE REF TO if_oo_adt_classrun_out.

  PROTECTED SECTION.
  PRIVATE SECTION.

    DATA: out TYPE REF TO if_oo_adt_classrun_out.

ENDCLASS.

CLASS zcl_work_order_crud_handlerjcc IMPLEMENTATION.

  METHOD constructor.

    me->out = out.

  ENDMETHOD.

  METHOD if_oo_adt_classrun~main.

**   Lectura orden de trabajo
     me->read_work_order( iv_id_work_order = 123456789 ).

**   Creacion orden de trabajo
*    me->create_work_order(
*     EXPORTING
*        iv_id_work_order = 123456789
*        iv_id_customer   = 'CUST999'
*        iv_id_technician = 'TECH999'
*        iv_priority   = 'B'
*        out           = out ).

**   Actualizacion orden de trabajo

**   Eliminacion orden de trabajo


  ENDMETHOD.


  METHOD read_work_order.

    TRY.

        SELECT * FROM ztwork_order_jcc
        WHERE id_work_order EQ @iv_id_work_order
        INTO @DATA(ls_work_order).
        ENDSELECT.

        IF  sy-subrc EQ 0.
*        out->write( name = 'work_order READ' data = ls_work_order ).
          me->out->write( name = 'work_order READ' data = ls_work_order ).
        ENDIF.
      CATCH cx_sy_open_sql_db INTO DATA(lx_error).
        out->write( |Error SQL: { lx_error->get_text( ) }| ).
        RETURN.

    ENDTRY.

  ENDMETHOD.

  METHOD create_work_order.


    TRY.
        INSERT ztwork_order_jcc FROM TABLE @(  VALUE #(  (  id_customer = iv_id_customer
                                                            id_technicial = iv_id_technician
                                                            creation_date = cl_abap_context_info=>get_system_date(  )
                                                            status = 'PE'
                                                            priority = iv_priority
                                                            description = 'Orden de trabajo asignada' ) ) ).
        IF  sy-subrc EQ 0.
          out->write( |Orden insertada correctamente. Registro: { sy-dbcnt }| ).
        ENDIF.
      CATCH cx_sy_open_sql_db INTO DATA(lx_error).
        out->write( |Error SQL: { lx_error->get_text( ) }| ).
        RETURN.
    ENDTRY.

  ENDMETHOD.

  METHOD update_work_order.

    TRY.
        UPDATE ztwork_order_jcc FROM TABLE @(  VALUE #(  ( id_work_order = iv_id_work_order
                                                            id_technicial = iv_id_technician
                                                            creation_date = cl_abap_context_info=>get_system_date(  )
                                                            status = 'PE'
                                                            priority = iv_priority
                                                            description = 'Orden de trabajo actualizada' ) ) ).
        IF  sy-subrc EQ 0.
          out->write( name = 'work_order UPDATE' data = sy-dbcnt ).
        ENDIF.
      CATCH cx_sy_open_sql_db INTO DATA(lx_error).
        out->write( |Error SQL: { lx_error->get_text( ) }| ).
        RETURN.
    ENDTRY.

  ENDMETHOD.

  METHOD delete_work_order.

    TRY.
        DELETE ztwork_order_jcc FROM TABLE @(  VALUE #(  ( id_work_order = iv_id_work_order ) ) ).
        IF  sy-subrc EQ 0.
          out->write( name = 'work_order DELETE' data = sy-dbcnt ).
        ENDIF.
      CATCH cx_sy_open_sql_db INTO DATA(lx_error).
        out->write( |Error SQL: { lx_error->get_text( ) }| ).
        RETURN.
    ENDTRY.


  ENDMETHOD.

ENDCLASS.
