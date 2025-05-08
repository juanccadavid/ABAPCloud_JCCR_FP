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
                                      iv_id_customer   TYPE zde_costumer_id_jccr
                                      iv_status        TYPE zde_status_jccr
                                      out              TYPE REF TO if_oo_adt_classrun_out
                            RETURNING VALUE(rv_valid)  TYPE abap_bool,

      validate_delete_order IMPORTING iv_id_work_order TYPE zde_work_order_id_jccr
                                      iv_status        TYPE zde_status_jccr
                                      out              TYPE REF TO if_oo_adt_classrun_out
                            RETURNING VALUE(rv_valid)  TYPE abap_bool,

      validate_status_and_priority IMPORTING iv_status        TYPE zde_status_jccr
                                             iv_priority      TYPE zde_priority_jccr
                                             out              TYPE REF TO if_oo_adt_classrun_out
                                   RETURNING VALUE(rv_valid)  TYPE abap_bool,
      constructor.

  PROTECTED SECTION.
  PRIVATE SECTION.

    DATA: lx_error TYPE REF TO cx_sy_open_sql_db.

    "Lllamar la clase con los metodos CRUD
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
                          RETURNING VALUE(rv_exists) TYPE abap_bool,
      check_status IMPORTING iv_status        TYPE zde_status_jccr
                             out              TYPE REF TO if_oo_adt_classrun_out
                   RETURNING VALUE(rv_exists) TYPE abap_bool,
      check_priority IMPORTING iv_priority      TYPE zde_priority_jccr
                             out              TYPE REF TO if_oo_adt_classrun_out
                   RETURNING VALUE(rv_exists) TYPE abap_bool.

      "Estructura de catalogos
      CONSTANTS : BEGIN OF mc_valid_status,
                    pending   TYPE zde_status_jccr VALUE 'PE',
                    completed TYPE zde_status_jccr VALUE 'CO',
                  END OF mc_valid_status,

                  BEGIN OF mc_valid_priority,
                    high TYPE zde_priority_jccr VALUE 'A',
                    low  TYPE zde_priority_jccr VALUE 'B',
                  END OF mc_valid_priority.

     DATA: mt_valid_status   TYPE RANGE OF zde_status_jccr,
           mT_valid_priority TYPE RANGE OF zde_priority_jccr.

ENDCLASS.


CLASS zcl_work_order_validator_jccr IMPLEMENTATION.

METHOD constructor.
mt_valid_status = VALUE #( ( sign = 'I'
                             option = 'EQ'
                             low = mc_valid_status-completed )
                           ( sign = 'I'
                             option = 'EQ'
                             low = mc_valid_status-pending ) ).

mt_valid_priority = VALUE #( ( sign = 'I'
                             option = 'EQ'
                             low = mc_valid_priority-high )
                           ( sign = 'I'
                             option = 'EQ'
                             low = mc_valid_priority-low ) ).
ENDMETHOD.

  METHOD if_oo_adt_classrun~main.

    IF go_crud IS INITIAL.
      go_crud = NEW zcl_work_order_crud_handlerjcc( out ).
    ENDIF.

    DATA: lv_id_work_order TYPE zde_work_order_id_jccr VALUE '0323456789',
          lv_id_customer   TYPE zde_costumer_id_jccr VALUE '2',
          lv_id_technician TYPE zde_technician_id_jccr VALUE '01',
          lv_status        TYPE zde_status_jccr VALUE 'PE',
          lv_priority      TYPE zde_priority_jccr VALUE 'A',
          lv_description   TYPE zde_description_jccr VALUE 'Nueva orden',

          lv_name_client TYPE string VALUE 'Fep Cadavid',
          lv_address_client TYPE string VALUE 'Calle 5 S N 8',
          lv_phone_client TYPE char13 VALUE 3126911427,

          lv_name_technicial TYPE string VALUE 'Diana Restrepo',
          lv_name_specialty TYPE string VALUE 'Sales Specialist'.

    "OPERACION PRINCIPAL
    DATA(lv_operacion) = 'READ'.

    CASE lv_operacion.

      WHEN 'CREARCLIENTE'.
        TRY.
            INSERT ztcustomer_jcc FROM TABLE @(  VALUE #(  (  id_customer = lv_id_customer
                                                                name = lv_name_client
                                                                address = lv_address_client
                                                                phone = lv_phone_client ) ) ).
            IF  sy-subrc EQ 0.
              out->write( |Cliente creado correctamente. Registro: { sy-dbcnt }| ).
            ENDIF.
          CATCH cx_sy_open_sql_db INTO lx_error.
            out->write( |Error SQL: { lx_error->get_text( ) }| ).
            RETURN.
        ENDTRY.

      WHEN 'ELIMINARCLIENTE'.
        TRY.
            DELETE ztcustomer_jcc FROM TABLE @(  VALUE #(  (  id_customer = lv_id_customer ) ) ).
            IF  sy-subrc EQ 0.
              out->write( |Cliente eliminado correctamente. Registro: { sy-dbcnt }| ).
            ENDIF.
          CATCH cx_sy_open_sql_db INTO lx_error.
            out->write( |Error SQL: { lx_error->get_text( ) }| ).
            RETURN.
        ENDTRY.

      WHEN 'CREARTECNICO'.

        TRY.
            INSERT zttechnician_jcc FROM TABLE @(  VALUE #(  (  id_technicial = lv_id_technician
                                                                name = lv_name_technicial
                                                                specialty = lv_name_specialty ) ) ).
            IF  sy-subrc EQ 0.
              out->write( |Tecnico creado correctamente. Registro: { sy-dbcnt }| ).
            ENDIF.
          CATCH cx_sy_open_sql_db INTO lx_error.
            out->write( |Error SQL: { lx_error->get_text( ) }| ).
            RETURN.
        ENDTRY.

      WHEN 'ELIMINARTECNICO'.

        TRY.
            DELETE zttechnician_jcc FROM TABLE @(  VALUE #(  (  id_technicial = lv_id_technician ) ) ).
            IF  sy-subrc EQ 0.
              out->write( |Tecnico eliminado correctamente. Registro: { sy-dbcnt }| ).
            ENDIF.
          CATCH cx_sy_open_sql_db INTO lx_error.
            out->write( |Error SQL: { lx_error->get_text( ) }| ).
            RETURN.
        ENDTRY.

    WHEN 'READ'.
      "   Leer orden de trabajo
      " Ejemplo de AUTHORITY-CHECK, se evalua el sy-subrc y si es correcto procede
*      AUTHORITY-CHECK OBJECT 'ZAOUSERJCC'
*      ID 'ZAFUSERJCC' FIELD lv_id_work_order
*      ID 'ACTVT' FIELD '03'.
*
*      IF sy-subrc EQ 0.
*        read_order( iv_id_work_order = lv_id_work_order
*                    out              = out ).
*      ELSE.
*      out->write( 'No tiene autorización para ejecutar la acción' ).
*      ENDIF.

      read_order( iv_id_work_order = lv_id_work_order
                    out              = out ).

     WHEN 'CREATE'.
      "   Creacion orden de trabajo

        IF me->validate_create_order(
                     iv_id_customer   = lv_id_customer
                     iv_id_technician = lv_id_technician
                     iv_priority      = lv_priority
                     out              = out ).

          go_crud->create_work_order(
           EXPORTING
                              iv_id_work_order = lv_id_work_order
                              iv_id_customer   = lv_id_customer
                              iv_id_technician = lv_id_technician
                              iv_status = lv_status
                              iv_priority   = lv_priority
                              iv_description = lv_description ).
        ENDIF.
        RETURN.


      WHEN 'UPDATE'.
        "   Actualizacion orden de trabajo

          IF me->validate_update_order(
                     iv_id_work_order   = lv_id_work_order
                     iv_id_customer   = lv_id_customer
                     iv_status      = lv_status
                     out              = out ).

             go_crud->update_work_order(
             EXPORTING
                               iv_id_work_order = lv_id_work_order
                               iv_id_customer   = lv_id_customer
                               iv_id_technician = lv_id_technician
                               iv_status = lv_status
                               iv_priority   = lv_priority
                               iv_description = lv_description ).
           ENDIF.
           RETURN.


    WHEN 'DELETE'.
      "   Eliminacion orden de trabajo

        IF me->validate_delete_order(
                  iv_id_work_order   = lv_id_work_order
                  iv_status      = lv_status
                  out              = out ).

          go_crud->delete_work_order(
          EXPORTING
                            iv_id_work_order = lv_id_work_order ).

        ENDIF.
        RETURN.

      WHEN 'ESTADOYPRIORIDAD'.
        "   Estado y prioridad
           IF me->validate_status_and_priority(
                     iv_status      = lv_status
                     iv_priority   = lv_priority
                     out              = out ).
           ENDIF.
           RETURN.
    ENDCASE.


  ENDMETHOD.

  METHOD read_order.

    go_crud->read_work_order(
      EXPORTING
        iv_id_work_order   = iv_id_work_order ).

  ENDMETHOD.

  METHOD validate_create_order.

    CLEAR rv_valid.

    " Validación de parámetros
    IF iv_id_customer  IS INITIAL OR
       iv_id_technician IS INITIAL OR
       iv_priority IS INITIAL.
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

    IF iv_priority NOT IN mt_valid_priority.
      rv_valid = abap_false.
      RETURN.
    ENDIF.

    rv_valid = abap_true.

  ENDMETHOD.

  METHOD validate_update_order.

    CLEAR rv_valid.

    " Validación de parámetros
    IF iv_id_work_order   IS INITIAL OR
       iv_status IS INITIAL.
      rv_valid = abap_false.
      RETURN.
    ENDIF.

    IF check_order_exists( iv_id_work_order = iv_id_work_order
                              out            = out            ) = abap_false.
      out->write( 'Order not exists' ).
      RETURN.
    ENDIF.

    IF iv_status EQ mc_valid_status-completed.
      rv_valid = abap_false.
      RETURN.
    ENDIF.

    rv_valid = abap_true.

  ENDMETHOD.

  METHOD validate_delete_order.

    CLEAR rv_valid.

    " Validación de parámetros
    IF iv_status IS INITIAL OR
       iv_id_work_order IS INITIAL.
      rv_valid = abap_false.
      RETURN.
    ENDIF.

    IF iv_status NE mc_valid_status-pending.
      rv_valid = abap_false.
      RETURN.
    ENDIF.

    IF check_order_history( iv_id_work_order = iv_id_work_order
                              out            = out            ) = abap_false.
      out->write( 'Order have history' ).
      RETURN.
    ENDIF.

    rv_valid = abap_true.

  ENDMETHOD.


  METHOD validate_status_and_priority.

    CLEAR rv_valid.

    IF iv_status IS INITIAL OR
       iv_priority IS INITIAL.
      rv_valid = abap_false.
      RETURN.
    ENDIF.

    IF iv_status NOT IN mt_valid_status.
      rv_valid = abap_false.
      RETURN.
    ENDIF.

    IF iv_priority NOT IN mt_valid_priority.
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

          out->write( name = 'Customer: '
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
          out->write( name = 'technicial: '
                      data = ls_technicial ).
        ENDIF.
      CATCH cx_sy_open_sql_db INTO DATA(lx_error).
        out->write( |Error SQL: { lx_error->get_text( ) }| ).
        RETURN.
    ENDTRY.

  ENDMETHOD.

  METHOD check_order_exists.

    CLEAR rv_exists.

    TRY.
        SELECT * FROM ztwork_order_jcc
        WHERE id_work_order EQ @iv_id_work_order
        INTO @DATA(ls_work_order).
        ENDSELECT.
        IF sy-subrc EQ 0.
          rv_exists = abap_true.
          out->write( name = 'work_order: '
                      data = ls_work_order ).
        ENDIF.
      CATCH cx_sy_open_sql_db INTO DATA(lx_error).
        out->write( |Error SQL: { lx_error->get_text( ) }| ).
        RETURN.
    ENDTRY.

  ENDMETHOD.

  METHOD check_order_history.

    CLEAR rv_exists.

    TRY.

        SELECT * FROM ztwork_orderhjcc
        WHERE id_work_order EQ @iv_id_work_order
        INTO @DATA(ls_work_order).
        ENDSELECT.
        IF sy-subrc EQ 0.
          rv_exists = abap_true.
          out->write( name = 'work_order history: '
                      data = ls_work_order ).
        ENDIF.
      CATCH cx_sy_open_sql_db INTO DATA(lx_error).
        out->write( |Error SQL: { lx_error->get_text( ) }| ).
        RETURN.
    ENDTRY.


  ENDMETHOD.

METHOD check_status.

  CLEAR rv_exists.

  IF iv_status NOT IN mt_valid_status.
    rv_exists = abap_false.
    RETURN.
  ENDIF.

  rv_exists = abap_true.

ENDMETHOD.

METHOD check_priority.

  CLEAR rv_exists.

  IF iv_priority NOT IN mt_valid_priority.
    rv_exists = abap_false.
    RETURN.
  ENDIF.

  rv_exists = abap_true.

ENDMETHOD.
ENDCLASS.
