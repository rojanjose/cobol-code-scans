IDENTIFICATION DIVISION.                                         
PROGRAM-ID. COBABEND.                                               
PROCEDURE DIVISION.

EXEC CICS HANDLE ABEND
    LABEL (X0000-HANDLE-ABEND-PARA)
END-EXEC.

X0000-HANDLE-ABEND-PARA.
DISPLAY 'Program Abended'.
