       IDENTIFICATION DIVISION.
       PROGRAM-ID. Villamil01.

       DATA DIVISION.
         WORKING-STORAGE SECTION.
           01 students.
             02 student-info OCCURS 5 TIMES.
               03 Deleted PIC 9 VALUE 1.
               03 fullname PIC X(30).
               03 sNo PIC X(30).
               03 course PIC X(20).
               03 contact-info.
                 04 mobile PIC X(11).
                 04 landline PIC X(8).
               03 age PIC 99.
           77 EXITED PIC 9 VALUE 0.
           77 IT PIC 99 VALUE 1.
           77 CHOICE PIC 9.
           77 NUMstd PIC 9 VALUE 0.
           77 STDchoice PIC 9 VALUE 1.
           77 STDsearch PIC X(30).

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
               PERFORM ADDSTUDENT
             ELSE
               IF CHOICE = 2 THEN
                 DISPLAY "E"
               ELSE
                 IF CHOICE = 3 THEN
                   DISPLAY "A"
                 ELSE
                   IF CHOICE = 4 THEN
                     DISPLAY "C"
                     PERFORM VIEWSTUDENT
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

         CHOOSESTD.
          IF Deleted(IT) = 0 THEN
            COMPUTE IT = IT + 1
          ELSE
            DISPLAY "Student slot " WITH NO ADVANCING
            DISPLAY IT WITH NO ADVANCING
            DISPLAY " is occupied"
          END-IF.
         
         FINDSTD.
          IF STDsearch IS NOT EQUAL TO sNo(IT) THEN
            COMPUTE IT = IT + 1
          END-IF.

         ADDSTUDENT.
           IF NUMstd < 5 THEN
             MOVE 1 TO IT
             PERFORM CHOOSESTD
             DISPLAY IT
             DISPLAY "Enter student number: " WITH NO ADVANCING
             ACCEPT sNo(IT)
             DISPLAY "Enter student full name: " WITH NO ADVANCING
             ACCEPT fullname(IT)
             DISPLAY "Enter student course: " WITH NO ADVANCING
             ACCEPT course(IT)
             DISPLAY "Enter student mobile no. : " WITH NO ADVANCING
             ACCEPT mobile(IT)
             DISPLAY "Enter student landline no. : " WITH NO ADVANCING
             ACCEPT landline(IT)
             DISPLAY "Enter student age: " WITH NO ADVANCING
             ACCEPT age(IT)
             COMPUTE NUMstd = NUMstd + 1
             MOVE 1 TO IT
           ELSE
             DISPLAY "The student directory is full"
           END-IF.

         VIEWSTUDENT.
           IF NUMstd > 0 THEN
             DISPLAY "Enter his/her student number: " WITH NO ADVANCING
             ACCEPT STDsearch
             PERFORM FINDSTD UNTIL STDsearch IS EQUAL TO sNo(IT)
             DISPLAY "student number: " WITH NO ADVANCING
             DISPLAY sNo(IT)
             DISPLAY "student full name: " WITH NO ADVANCING
             DISPLAY fullname(IT)
             DISPLAY "student course: " WITH NO ADVANCING
             DISPLAY course(IT)
             DISPLAY "student mobile no. : " WITH NO ADVANCING
             DISPLAY mobile(IT)
             DISPLAY "student landline no. : " WITH NO ADVANCING
             DISPLAY landline(IT)
             DISPLAY "student age: " WITH NO ADVANCING
             DISPLAY age(IT)
             MOVE 1 TO IT
           ELSE
             DISPLAY "There are no students to view in the directory"
           END-IF.
        END PROGRAM.