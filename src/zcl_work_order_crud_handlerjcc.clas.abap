CLASS zcl_work_order_crud_handlerjcc DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.

  INTERFACES : if_oo_adt_classrun.

  METHODS:
    "CRUD
    create_work_order IMPORTING io_out TYPE REF TO if_oo_adt_classrun_out,
    read_work_order IMPORTING io_out TYPE REF TO if_oo_adt_classrun_out,
    update_work_order IMPORTING io_out TYPE REF TO if_oo_adt_classrun_out,
    delete_work_order IMPORTING io_out TYPE REF TO if_oo_adt_classrun_out.

  PROTECTED SECTION.
  PRIVATE SECTION.
ENDCLASS.

CLASS zcl_work_order_crud_handlerjcc IMPLEMENTATION.

METHOD if_oo_adt_classrun~main.

*   Creacion orden de trabajo
    create_work_order( out ).

*   Lectura orden de trabajo
    read_work_order( out ).

*   Actualizacion orden de trabajo
    update_work_order( out ).

*   Eliminacion orden de trabajo
    delete_work_order( out ).
ENDMETHOD.

METHOD create_work_order.

ENDMETHOD.

METHOD read_work_order.

ENDMETHOD.

METHOD update_work_order.

ENDMETHOD.

METHOD delete_work_order.

ENDMETHOD.

ENDCLASS.
