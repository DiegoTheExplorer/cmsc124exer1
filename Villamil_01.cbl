       IDENTIFICATION DIVISION.
       PROGRAM-ID. Villamil01.

       DATA DIVISION.
         WORKING-STORAGE SECTION.
           01 students.
             02 student-info OCCURS 5 TIMES.
               03 isEmpty PIC 9 VALUE 1.
               03 fullname PIC X(30).
               03 sNo PIC X(30).
               03 course PIC X(20).
               03 contact-info.
                 04 mobile PIC X(11).
                 04 landline PIC X(8).
               03 age PIC 99.
           77 EXITED PIC 9 VALUE 0.
           77 IT PIC 99.
           77 CHOICE PIC 9.
           77 NUMstd PIC 9 VALUE 0.

       PROCEDURE DIVISION.

         PERFORM MENU UNTIL CHOICE = 6.

         MENU.

		     DISPLAY "MENU".
			   DISPLAY "[1] ADD STUDENT".
			   DISPLAY "[2] EDIT STUDENT INFO".
			   DISPLAY "[3] DELETE STUDENT".
			   DISPLAY "[4] VIEW STUDNENT".
			   DISPLAY "[5] VIEW ALL STUDENTS".
			   DISPLAY "[6] EXIT".
			   DISPLAY "CHOICE: " WITH NO ADVANCING.
             ACCEPT CHOICE.

             IF CHOICE = 1 THEN
               DISPLAY "PREPARING TO ADD STUDENT...."
             ELSE
               IF CHOICE = 2 THEN
                 DISPLAY "E"
               ELSE
                 IF CHOICE = 3 THEN
                   DISPLAY "A"
                 ELSE
                   IF CHOICE = 4 THEN
                     DISPLAY "C"
                   ELSE
                     IF CHOICE = 5 THEN
                       DISPLAY "H"
                     ELSE
                       IF CHOICE = 6 THEN
                         DISPLAY "ES"
                         STOP RUN
                       ELSE
                         DISPLAY "Invalid choice, try again"
                       END-IF
                     END-IF
                   END-IF
                 END-IF
               END-IF
             END-IF.



         ISFULL.
           IF isEmpty(IT) = 0 THEN
             ADD 1 TO NUMstd GIVING NUMstd
           END-IF.

         ADDSTUDENT.
           DISPLAY "Enter studnet number: " WITH NO ADVANCING.
         






