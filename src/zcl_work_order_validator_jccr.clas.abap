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
                                   RETURNING VALUE(rv_valid)  TYPE abap_bool.

  PROTECTED SECTION.
  PRIVATE SECTION.

    CONSTANTS: c_valid_status   TYPE string VALUE 'PE CO',
               c_valid_priority TYPE string VALUE 'A B'.

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
                          RETURNING VALUE(rv_exists) TYPE abap_bool.

ENDCLASS.



CLASS zcl_work_order_validator_jccr IMPLEMENTATION.


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
    DATA(lv_operacion) = 'DELETE'.

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

      AUTHORITY-CHECK OBJECT 'ZAOUSERJCC'
      ID 'ZAFUSERJCC' FIELD lv_id_work_order
      ID 'ACTVT' FIELD '03'.

      IF sy-subrc EQ 0.
        read_order( iv_id_work_order = lv_id_work_order
                    out              = out ).
      ELSE.
      out->write( 'No tiene autorización para ejecutar la acción' ).
      ENDIF.

     WHEN 'CREATE'.
      "   Creacion orden de trabajo

      AUTHORITY-CHECK OBJECT 'ZAOUSERJCC'
      id 'ZAFUSERJCC' field lv_id_work_order
      id 'ACTVT' field '01'.

      IF sy-subrc EQ 0.
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
           ELSE.
           out->write( 'No tiene autorización para ejecutar la acción' ).
      ENDIF.


      WHEN 'UPDATE'.
        "   Actualizacion orden de trabajo

      AUTHORITY-CHECK OBJECT 'ZAOUSERJCC'
      id 'ZAFUSERJCC' field lv_id_work_order
      id 'ACTVT' field '02'.

      IF sy-subrc EQ 0.
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
         ELSE.
           out->write( 'No tiene autorización para ejecutar la acción' ).
     ENDIF.


    WHEN 'DELETE'.
      "   Eliminacion orden de trabajo
      AUTHORITY-CHECK OBJECT 'ZAOUSERJCC'
      ID 'ZAFUSERJCC' FIELD lv_id_work_order
      ID 'ACTVT' FIELD '06'.
      IF sy-subrc EQ 0.
        IF me->validate_delete_order(
                  iv_id_work_order   = lv_id_work_order
                  iv_status      = lv_status
                  out              = out ).

          go_crud->delete_work_order(
          EXPORTING
                            iv_id_work_order = lv_id_work_order ).

        ENDIF.
        RETURN.
      ELSE.
        out->write( 'No tiene autorización para ejecutar la acción' ).
      ENDIF.


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

    IF iv_priority NE 'A' AND iv_priority NE 'B'.
      out->write( 'Invalid priority' ).
      RETURN.
    ENDIF.

    rv_valid = abap_true.

  ENDMETHOD.

  METHOD validate_update_order.

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

    IF iv_status NE 'PE'.
      out->write( 'Invalid status' ).
      RETURN.
    ENDIF.

    rv_valid = abap_true.

  ENDMETHOD.

  METHOD validate_delete_order.

    " Validación de parámetros
    IF iv_status IS INITIAL OR
       iv_id_work_order IS INITIAL.
      rv_valid = abap_false.
      RETURN.
    ENDIF.

        IF iv_status NE 'PE'.
          out->write( 'rder status is not pending' ).
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

    IF iv_status IS INITIAL OR
       iv_priority IS INITIAL.
      rv_valid = abap_false.
      RETURN.
    ENDIF.

    IF iv_status NE 'PE' AND iv_status NE 'CO'.
      out->write( 'Status is not valid' ).
      RETURN.
    ENDIF.

    IF iv_priority NE 'A' AND iv_priority NE 'B'.
      out->write( 'Priority is not valid' ).
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

ENDCLASS.
