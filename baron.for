      PROGRAM BARON
      DOUBLE PRECISION P,S,H,E,Y,A,I,Q,D,C
	  
C     1.90 S P=95;S S=2800;S H=3000;S E=200;S Y=3;S A=1000;S I=5;S Q=1

      P=95.0
      S=2800.0
      H=3000.0
      E=200.0
      Y=3.0
      A=1000.0
      I=5.0
      Q=1.0

    5 WRITE(*,*) ""
      CALL GREET()
      D=0
    7 CALL TEN()
	  WRITE(*,*) "------------------------------"
      WRITE(*,*) CHAR(10),"LAST YEAR"
	  WRITE(*,*) "= ",D,"  STARVED,"
	  CALL TEN()
	  WRITE(*,*) "= ",I,"  ARRIVED,"
	  P=P+I
	  
	  IF(250-P .LT. 0) THEN
	  WRITE(10,*) CHAR(10),CHAR(10),"   YOU HAVE BEEN PROMOTED TO PM!",CHAR(10),CHAR(10)
	  GO TO 20
	  END IF
	  
	  IF(-1*Q .LT. 0) GO TO 10
	  
	  P=P/2
	  WRITE(*,*) CHAR(10),CHAR(10),"**PLAGUE**",CHAR(10),CHAR(10)
	  
   10 WRITE(*,*) ""
      WRITE(*,*) "POPULATION IS  ",P
	  WRITE(*,*) CHAR(10),"THE CITY OWNS=",A," ACRES",CHAR(10)
	  
C   I (H-1Z)2.5 ?????

      WRITE(*,*) "WE HARVESTED=",Y," BUSHELS PER ACRE;"
	  C=1
	  
      WRITE(*,*) "RATS ATE=",E," BUSHELS,YOU NOW HAVE"
	  WRITE(*,*) "=",S," BUSHELS IN STORE."
	  
	  Y=(8*RAND(0))+17
	  
	  WRITE(*,'(A$)') CHAR(10),'LORD BARON: LAND IS TRADING AT='
	  WRITE(*,*) Y, " BUSHELS PER ACRE;"
	  C=1
   20 WRITE(*,'(A$)') "HOW MANY ACRES OF LAND DO YOU WITH TO BUY: "
	  READ(*,*) Q
	  
C  REPEAT BUY QUESTION IF BUY ANS IS NEG	  
	  IF(Q .LT. 0) THEN
	  CALL JEST()
	  GO TO 20
	  END IF
C  IF BUY ANS IS POS -JUMP
      IF(Q .GT. 0) THEN
	  GO TO 30
	  
C  IF BUY ANS IS 0 - GO INTO SELL		  
	  IF(Q .EQ. 0) THEN
   25 WRITE(*,'(A$)') "TO SELL: "
	  READ(*,*) Q
	  END IF
C  IF SELL ANS IS NEG REPEAT SELL QUESTN	  
	  IF(Q .LT. 0) THEN
	  CALL JEST()
	  GO TO 25
	  END IF
C  IF SELL == 0 GO TO SECTION 4	  
	  IF(Q .EQ. 0) THEN
	  GO TO 40
      END IF
C  IF SELL IS POS ..	  
	  Q=(-1)*Q
	  IF(A+Q .LT. 0) THEN
      WRITE(*,*) ""
	  END IF
	  
   30 IF(Y*Q-S .LT. 0) THEN
	  GO TO 40
	  END IF
	  IF(Y*Q-S .EQ. 0) THEN
      WRITE(*,*) "DO NO SEED STUFF..."	  
	  END IF
	  IF(Y*Q-S .GT. 0) THEN
	  WRITE(*,*) CHAR(10),"LORD BARON: BUT YOU HAVE ONLY"
	  WRITE(*,*) "=",S,"BUSHELS IN STORE."
	  WRITE(*,*) CHAR(10),"YOU BUY AT MOST=",S/Y-1,CHAR(10)
	  GO TO 20
	  END IF
	  END IF

C 3.9	  
   40 A=A+Q
	  S=S-Y*Q
	  C=0

C BEGIN SECTION 4.1
   50 WRITE(*,'(A$)') "BUSHELS TO USE AS FOOD: "
	  READ(*,*) Q
	  IF(Q .LT. 0) THEN
	  CALL JEST()
	  GO TO 50
	  END IF
	  IF(Q .EQ. 0) THEN
	  WRITE(*,*) CHAR(10),CHAR(10),CHAR(10),"REVOLUTION!!!",CHAR(10),CHAR(10),CHAR(10)
	  GO TO 100
	  END IF
	  
	  IF(Q-S .LT. 0) THEN
	  S=S-Q
	  C=1
	  WRITE(*,'(A$)') "HOW MANY ACRES OF LAND DO YOU WISH TO"
      WRITE(*,'(A$)') "PLANT WITH SEED "
	  READ(*,*) D
C DO STUFF HERE
	  END IF
	  IF(Q-S .EQ. 0) THEN
	  S=S-Q
	  C=1
	  WRITE(*,*) "LORD BARON: YOU HAVE NO GRAIN LEFT AS SEED  !!!"
	  D=0
	  GO TO 60
	  END IF

   60 S=S-(D/2)
      C=(5*(RAND(1)))+1
	  Y=C
	  H=D*Y
	  C=(5*(RAND(1)))+1
	  E=0
	  IF(((C/2)-C/2) .LT. 0) THEN
	  GO TO 70
	  END IF
	  E=S/C
   70 S=S-E+H
      C=(5*(RAND(1)))+1
      I=(C*(20*A+S)/P/100+1)
      C=(Q/20) 
	  Q=(10*(RAND(1)))
	  IF(P-C .LT. 0) THEN
	  GO TO 5
	  END IF
	  D=P-C
	  P=C
	  GO TO 7
	  
  100 STOP
      END
	  
C THIS PRINTS THE "BARON:" GREETING (6.10)
      SUBROUTINE GREET()

   10 FORMAT(//'LORD BARON:   '/)
      WRITE(*,10)

      RETURN
      END
	  
	  
C 7.20 JEST MESSAGE
      SUBROUTINE JEST()
	  WRITE(*,'(A$)') CHAR(10),"PLEASE, LORD BARON. I AM NOT"
	  WRITE(*,*) "IN A MOOD FOR JESTING.",CHAR(10)
	  RETURN
	  END

	  
C 10.10
      SUBROUTINE TEN()
      REAL(4) :: O=34, F=58
	  INTEGER ZF
	  
	  ZF=1+INT(ALOG(O)/ALOG(F))
	  
	  RETURN
	  END