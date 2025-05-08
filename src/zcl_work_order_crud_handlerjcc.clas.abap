CLASS zcl_work_order_crud_handlerjcc DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.

    METHODS:
      constructor IMPORTING out TYPE REF TO if_oo_adt_classrun_out.

    INTERFACES : if_oo_adt_classrun.

    METHODS:
      "CRUD
      read_work_order IMPORTING iv_id_work_order TYPE zde_work_order_id_jccr,

      create_work_order IMPORTING iv_id_work_order TYPE zde_work_order_id_jccr
                                  iv_id_customer   TYPE zde_costumer_id_jccr
                                  iv_id_technician TYPE zde_technician_id_jccr
                                  iv_status        TYPE zde_status_jccr
                                  iv_priority      TYPE zde_priority_jccr
                                  iv_description   TYPE zde_description_jccr,

      update_work_order IMPORTING iv_id_work_order TYPE zde_work_order_id_jccr
                                  iv_id_customer   TYPE zde_costumer_id_jccr
                                  iv_id_technician TYPE zde_technician_id_jccr
                                  iv_status        TYPE zde_status_jccr
                                  iv_priority      TYPE zde_priority_jccr
                                  iv_description   TYPE zde_description_jccr,

      delete_work_order IMPORTING iv_id_work_order TYPE zde_work_order_id_jccr.

  PROTECTED SECTION.
  PRIVATE SECTION.

    DATA: out TYPE REF TO if_oo_adt_classrun_out.

    DATA: lx_error TYPE REF TO cx_sy_open_sql_db.

    "Para insertar en bd de historial
    DATA: lv_max_id TYPE ztwork_orderhjcc-id_history,
          lv_new_id TYPE ztwork_orderhjcc-id_history.

ENDCLASS.

CLASS zcl_work_order_crud_handlerjcc IMPLEMENTATION.

  METHOD constructor.

    me->out = out.

  ENDMETHOD.

  METHOD if_oo_adt_classrun~main.

*

  ENDMETHOD.


  METHOD read_work_order.

    TRY.

        SELECT * FROM ztwork_order_jcc
        WHERE id_work_order EQ @iv_id_work_order
        INTO @DATA(ls_work_order).
        ENDSELECT.

        IF  sy-subrc EQ 0.
          me->out->write( name = 'work_order READ' data = ls_work_order ).
        ENDIF.
      CATCH cx_sy_open_sql_db INTO DATA(lx_error).
        out->write( |Error SQL: { lx_error->get_text( ) }| ).
        RETURN.

    ENDTRY.

  ENDMETHOD.

  METHOD create_work_order.


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

    SELECT MAX( id_history )
    FROM ztwork_orderhjcc
    INTO @DATA(lv_max_id).

    lv_new_id = lv_max_id + 1.

    TRY.
        INSERT ztwork_orderhjcc FROM TABLE @(  VALUE #(  (  id_history = lv_new_id
                                                            id_work_order = iv_id_work_order
                                                            date_modification = cl_abap_context_info=>get_system_date(  )
                                                            description_change = iv_description ) ) ).
        IF  sy-subrc EQ 0.
          out->write( |Historial insertado correctamente. Registro: { sy-dbcnt }| ).
        ENDIF.
      CATCH cx_sy_open_sql_db INTO DATA(lx_errorr).
        out->write( |Error SQL: { lx_errorr->get_text( ) }| ).
        RETURN.
    ENDTRY.


  ENDMETHOD.

  METHOD update_work_order.

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

    SELECT MAX( id_history )
    FROM ztwork_orderhjcc
    INTO @DATA(lv_max_id).

    lv_new_id = lv_max_id + 1.

    TRY.
        INSERT ztwork_orderhjcc FROM TABLE @(  VALUE #(  (  id_history = lv_new_id
                                                            id_work_order = iv_id_work_order
                                                            date_modification = cl_abap_context_info=>get_system_date(  )
                                                            description_change = iv_description ) ) ).
        IF  sy-subrc EQ 0.
          out->write( |Historial actualizado correctamente. Registro: { sy-dbcnt }| ).
        ENDIF.
      CATCH cx_sy_open_sql_db INTO DATA(lx_errorr).
        out->write( |Error SQL: { lx_errorr->get_text( ) }| ).
        RETURN.
    ENDTRY.

  ENDMETHOD.

  METHOD delete_work_order.

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
