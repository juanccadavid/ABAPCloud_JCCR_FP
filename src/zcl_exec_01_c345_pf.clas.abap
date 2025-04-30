CLASS zcl_exec_01_c345_pf DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.

  INTERFACES: if_oo_adt_classrun.

  METHODS:

    create_records IMPORTING io_out TYPE REF TO if_oo_adt_classrun_out,
    read_records IMPORTING io_out TYPE REF TO if_oo_adt_classrun_out,
    update_records IMPORTING io_out TYPE REF TO if_oo_adt_classrun_out,
    delete_records IMPORTING io_out TYPE REF TO if_oo_adt_classrun_out.

    protected SECTION.
  PRIVATE SECTION.
ENDCLASS.



CLASS zcl_exec_01_c345_pf IMPLEMENTATION.

METHOD if_oo_adt_classrun~main.

*    Create records
*    create_records( out ).

*    Read records
*    read_records( out ).

*    Update records
*    update_records( out ).

*    Delete records
*    delete_records( out ).

ENDMETHOD.

METHOD create_records.
io_out->write( 'Add records' ).
ENDMETHOD.

METHOD read_records.

ENDMETHOD.

METHOD update_records.

ENDMETHOD.

METHOD delete_records.

ENDMETHOD.

ENDCLASS.
