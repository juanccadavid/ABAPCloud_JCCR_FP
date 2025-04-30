CLASS zcl_work_order_crud_handlertes DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.

    INTERFACES : if_oo_adt_classrun.

  METHODS:
    "CRUD
    test_create_work_order IMPORTING io_out TYPE REF TO if_oo_adt_classrun_out,
    test_read_work_order IMPORTING io_out TYPE REF TO if_oo_adt_classrun_out,
    test_update_work_order IMPORTING io_out TYPE REF TO if_oo_adt_classrun_out,
    test_delete_work_order IMPORTING io_out TYPE REF TO if_oo_adt_classrun_out.


  PROTECTED SECTION.
  PRIVATE SECTION.
ENDCLASS.



CLASS zcl_work_order_crud_handlertes IMPLEMENTATION.

METHOD if_oo_adt_classrun~main.

*   Creacion orden de trabajo
    test_create_work_order( out ).

**   Actualizacion orden de trabajo
*    test_read_work_order( out ).

**   Eliminacion orden de trabajo
*    test_update_work_order( out ).

**   Estado y prioridad
*    test_delete_work_order( out ).

ENDMETHOD.

METHOD test_create_work_order.

ENDMETHOD.

METHOD test_read_work_order.

ENDMETHOD.

METHOD test_update_work_order.

ENDMETHOD.

METHOD test_delete_work_order.

ENDMETHOD.

ENDCLASS.
