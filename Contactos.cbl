      ******************************************************************
      * Author:
      * Date:
      * Purpose:
      * Tectonics: cobc
      ******************************************************************
       IDENTIFICATION DIVISION.
       PROGRAM-ID. GESTOR-CONTACTOS.

       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT CONTACTOS-FILE ASSIGN TO "contactos.dat"
           ORGANIZATION IS LINE SEQUENTIAL
           FILE STATUS IS FS-STATUS.

       DATA DIVISION.
       FILE SECTION.
       FD CONTACTOS-FILE.
       01 CONTACTO-REGISTRO.
           05 C-NOMBRE      PIC X(30).
           05 C-FECHA       PIC X(5).
           05 C-TELEFONO    PIC X(15).
           05 C-EMAIL       PIC X(30).

       WORKING-STORAGE SECTION.
       01 FS-STATUS        PIC XX.
           88 FS-OK        VALUE '00'.
           88 FS-NOT-FOUND VALUE '35'.
       01 OPCION           PIC 9.
       01 CONTACTOS-TABLE.
           05 CONTACTO OCCURS 100 TIMES INDEXED BY CONT-IDX.
               10 NOMBRE    PIC X(30).
               10 FECHA     PIC X(5).
               10 TELEFONO  PIC X(15).
               10 EMAIL     PIC X(30).
       01 CONTADOR         PIC 9(3) VALUE 0.
       01 MES-BUSCADO      PIC 99.
       01 ENCONTRADO       PIC X VALUE 'N'.
       01 EOF-FLAG         PIC X VALUE 'N'.
       01 TEMP-CONTACTO.
           05 T-NOMBRE     PIC X(30).
           05 T-FECHA      PIC X(5).
           05 T-TELEFONO   PIC X(15).
           05 T-EMAIL      PIC X(30).
       01 I                PIC 9(3).
       01 J                PIC 9(3).

       PROCEDURE DIVISION.
       MAIN-LOGIC.
           PERFORM CARGAR-CONTACTOS
           PERFORM MENU-PRINCIPAL UNTIL OPCION = 5
           PERFORM GUARDAR-CONTACTOS
           STOP RUN.

       CARGAR-CONTACTOS.
           OPEN INPUT CONTACTOS-FILE
           IF FS-NOT-FOUND
               DISPLAY "Archivo no encontrado. Se creará uno nuevo."
               MOVE 0 TO CONTADOR
           ELSE
               IF FS-OK
                   PERFORM LEER-ARCHIVO
               ELSE
                   DISPLAY "Error al abrir archivo: " FS-STATUS
                   STOP RUN
               END-IF
           END-IF
           CLOSE CONTACTOS-FILE.

       LEER-ARCHIVO.
           PERFORM UNTIL EOF-FLAG = 'S'
               READ CONTACTOS-FILE
                   AT END
                       MOVE 'S' TO EOF-FLAG
                   NOT AT END
                       ADD 1 TO CONTADOR
                       MOVE C-NOMBRE TO NOMBRE(CONTADOR)
                       MOVE C-FECHA TO FECHA(CONTADOR)
                       MOVE C-TELEFONO TO TELEFONO(CONTADOR)
                       MOVE C-EMAIL TO EMAIL(CONTADOR)
               END-READ
           END-PERFORM.

       GUARDAR-CONTACTOS.
           OPEN OUTPUT CONTACTOS-FILE
           PERFORM VARYING I FROM 1 BY 1 UNTIL I > CONTADOR
               MOVE NOMBRE(I) TO C-NOMBRE
               MOVE FECHA(I) TO C-FECHA
               MOVE TELEFONO(I) TO C-TELEFONO
               MOVE EMAIL(I) TO C-EMAIL
               WRITE CONTACTO-REGISTRO
           END-PERFORM
           CLOSE CONTACTOS-FILE.

       MENU-PRINCIPAL.
           DISPLAY " "
           DISPLAY "=== MENU PRINCIPAL ==="
           DISPLAY "1. Agregar contacto"
           DISPLAY "2. Buscar contacto por nombre"
           DISPLAY "3. Listar cumpleanios por mes"
           DISPLAY "4. Listar todos los contactos (ordenados)"
           DISPLAY "5. Salir"
           DISPLAY "Seleccione opcion (1-5): "
           ACCEPT OPCION

           EVALUATE OPCION
               WHEN 1 PERFORM AGREGAR-CONTACTO
               WHEN 2 PERFORM BUSCAR-CONTACTO
               WHEN 3 PERFORM LISTAR-CUMPLEANOS
               WHEN 4 PERFORM LISTAR-TODOS
           END-EVALUATE.

       AGREGAR-CONTACTO.
           IF CONTADOR >= 100
               DISPLAY "¡Error! No hay espacio para más contactos."
           ELSE
               ADD 1 TO CONTADOR
               DISPLAY "Nombre: "
               ACCEPT NOMBRE(CONTADOR)
               DISPLAY "Fecha cumpleaños (DD/MM): "
               ACCEPT FECHA(CONTADOR)
               DISPLAY "Telefono: "
               ACCEPT TELEFONO(CONTADOR)
               DISPLAY "Email: "
               ACCEPT EMAIL(CONTADOR)
               DISPLAY "Contacto agregado correctamente."
           END-IF.

       BUSCAR-CONTACTO.
           MOVE 'N' TO ENCONTRADO
           DISPLAY "Nombre a buscar: "
           ACCEPT T-NOMBRE

           PERFORM VARYING I FROM 1 BY 1 UNTIL I > CONTADOR
               IF NOMBRE(I) = T-NOMBRE
                   MOVE 'S' TO ENCONTRADO
                   DISPLAY "=== DATOS DEL CONTACTO ==="
                   DISPLAY "Nombre: " NOMBRE(I)
                   DISPLAY "Cumpleaños: " FECHA(I)
                   DISPLAY "Telefono: " TELEFONO(I)
                   DISPLAY "Email: " EMAIL(I)
               END-IF
           END-PERFORM

           IF ENCONTRADO = 'N'
               DISPLAY "Contacto no encontrado."
           END-IF.

       LISTAR-CUMPLEANOS.
           DISPLAY "Mes a buscar (1-12): "
           ACCEPT MES-BUSCADO

           DISPLAY "=CONTACTOS CON CUMPLEAÑOS EN MES " MES-BUSCADO " ="
           PERFORM VARYING I FROM 1 BY 1 UNTIL I > CONTADOR
               IF FECHA(I)(4:2) = MES-BUSCADO
                   DISPLAY "Nombre: " NOMBRE(I) " Fecha: " FECHA(I)
                   DISPLAY "Numero: " TELEFONO(I)
                   DISPLAY "Email: " EMAIL(I)
               END-IF
           END-PERFORM.

       LISTAR-TODOS.
           PERFORM ORDENAR-CONTACTOS
           DISPLAY "=== LISTA COMPLETA DE CONTACTOS ==="
           PERFORM VARYING I FROM 1 BY 1 UNTIL I > CONTADOR
               DISPLAY "Contacto #" I
               DISPLAY "Nombre: " NOMBRE(I)
               DISPLAY "Cumpleanios: " FECHA(I)
               DISPLAY "Telefono: " TELEFONO(I)
               DISPLAY "Email: " EMAIL(I)
               DISPLAY "---------------------"
           END-PERFORM.

       ORDENAR-CONTACTOS.
           PERFORM VARYING I FROM 1 BY 1 UNTIL I >= CONTADOR
               COMPUTE J = I + 1
               PERFORM UNTIL J > CONTADOR
                   IF NOMBRE(I) > NOMBRE(J)
                       MOVE NOMBRE(I) TO T-NOMBRE
                       MOVE FECHA(I) TO T-FECHA
                       MOVE TELEFONO(I) TO T-TELEFONO
                       MOVE EMAIL(I) TO T-EMAIL

                       MOVE NOMBRE(J) TO NOMBRE(I)
                       MOVE FECHA(J) TO FECHA(I)
                       MOVE TELEFONO(J) TO TELEFONO(I)
                       MOVE EMAIL(J) TO EMAIL(I)

                       MOVE T-NOMBRE TO NOMBRE(J)
                       MOVE T-FECHA TO FECHA(J)
                       MOVE T-TELEFONO TO TELEFONO(J)
                       MOVE T-EMAIL TO EMAIL(J)
                   END-IF
                   ADD 1 TO J
               END-PERFORM
           END-PERFORM.
