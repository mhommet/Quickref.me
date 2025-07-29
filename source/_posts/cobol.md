---
title: COBOL
date: 2025-07-29 10:32:03
background: bg-indigo-900
tags:
categories:
  - Programming
intro: |
  A quick reference guide to COBOL syntax and concepts.
plugins:
  - copyCode
---

## Program Structure

### COBOL Column Layout

COBOL utilise un format de colonnes strict (mode FIXED) ou libre (mode FREE) :

**Mode FIXED (traditionnel) :**

```cobol
*> Colonnes 1-6  : Numéros de séquence (optionnel)
*> Colonne 7     : Indicateur (*, /, D, -, etc.)
*> Colonnes 8-11 : Zone A (divisions, sections, paragraphes)
*> Colonnes 12-72: Zone B (instructions, données)
*> Colonnes 73-80: Identification (ignoré)

000100 IDENTIFICATION DIVISION.
000200 PROGRAM-ID. EXEMPLE.
000300 DATA DIVISION.
000400 WORKING-STORAGE SECTION.
000500 01  COMPTEUR    PIC 9(3).
000600 PROCEDURE DIVISION.
000700     DISPLAY "Hello World"
000800     STOP RUN.
```

**Mode FREE (moderne) :**

```cobol
>>SOURCE FORMAT IS FREE
IDENTIFICATION DIVISION.
PROGRAM-ID. EXEMPLE.
DATA DIVISION.
WORKING-STORAGE SECTION.
01 COMPTEUR PIC 9(3).
PROCEDURE DIVISION.
    DISPLAY "Hello World"
    STOP RUN.
```

### Basic Program Template

```cobol
IDENTIFICATION DIVISION.
PROGRAM-ID. MyProgram.

ENVIRONMENT DIVISION.

DATA DIVISION.
WORKING-STORAGE SECTION.
01 NAME     PIC A(30).

PROCEDURE DIVISION.
    DISPLAY "Hello" NAME
    STOP RUN.
```

### Four Main Divisions

| Division                  | Purpose                  |
| ------------------------- | ------------------------ |
| `IDENTIFICATION DIVISION` | Program identification   |
| `ENVIRONMENT DIVISION`    | System environment setup |
| `DATA DIVISION`           | Variable declarations    |
| `PROCEDURE DIVISION`      | Executable code          |

## Data Types & Variables {.row-span-2}

### Picture Clauses

| Syntax         | Data Type         | Example                |
| -------------- | ----------------- | ---------------------- |
| `PIC 9(n)`     | Positive integer  | `PIC 9(5)` → 00042     |
| `PIC S9(n)`    | Signed integer    | `PIC S9(5)` → -00123   |
| `PIC 9(n)V99`  | Decimal number    | `PIC 9(3)V99` → 123.45 |
| `PIC A(n)`     | Alphabetic only   | `PIC A(10)` → "MILAN"  |
| `PIC X(n)`     | Alphanumeric      | `PIC X(10)` → "ABC123" |
| `PIC +ZZZ9.99` | Formatted display | `+0123.45`             |

### Variable Declaration Examples

```cobol
01 AGE         PIC 99.        *> 2-digit integer
01 BALANCE     PIC S9(5)V99.  *> Signed decimal
01 NAME        PIC A(30).     *> Alpha string
01 CODE        PIC X(10).     *> Alphanumeric
```

### Group Data Items

```cobol
01 CLIENT.
   05 LAST-NAME   PIC A(30).
   05 FIRST-NAME  PIC A(30).
   05 AGE         PIC 99.
```

### Arrays (OCCURS)

```cobol
01 ACCOUNTS.
   05 ACCOUNT OCCURS 5 TIMES.
      10 ID          PIC 9(5).
      10 BALANCE     PIC 9(7)V99.
```

## Input/Output Operations

### Basic I/O

```cobol
DISPLAY "What is your name?"
ACCEPT NAME
DISPLAY "Hello " NAME
```

### File Operations

```cobol
SELECT FILE-DATA ASSIGN TO 'data.txt'
   ORGANIZATION IS LINE SEQUENTIAL.

FD FILE-DATA.
01 LINE-RECORD PIC X(80).

OPEN INPUT FILE-DATA
READ FILE-DATA
    AT END DISPLAY "End of file"
END-READ
CLOSE FILE-DATA
```

## Control Structures

### Conditional Statements

```cobol
IF AGE > 18
    MOVE "adult" TO STATUS
ELSE
    MOVE "minor" TO STATUS
END-IF
```

### EVALUATE (Switch-Case)

```cobol
EVALUATE CHOICE
    WHEN 1 DISPLAY "Option 1"
    WHEN 2 DISPLAY "Option 2"
    WHEN OTHER DISPLAY "Invalid"
END-EVALUATE
```

### Loops (PERFORM)

```cobol
PERFORM VARYING I FROM 1 BY 1 UNTIL I > 5
    DISPLAY "Round: " I
END-PERFORM
```

## Operations & Calculations

### MOVE Statement

```cobol
MOVE "John" TO NAME
MOVE ZEROS TO COUNTER
MOVE SPACES TO DESCRIPTION
```

### COMPUTE Statement

```cobol
COMPUTE VAT = PRICE * RATE / 100
COMPUTE TOTAL = PRICE + VAT
COMPUTE AVERAGE = TOTAL / COUNT
```

### Arithmetic Operations

```cobol
ADD 1 TO COUNTER
SUBTRACT TAX FROM GROSS-PAY
MULTIPLY HOURS BY RATE GIVING PAY
DIVIDE TOTAL BY COUNT GIVING AVERAGE
```

## Database Integration

### Embedded SQL (DB2)

```cobol
EXEC SQL
    SELECT BALANCE INTO :ACCOUNT-BALANCE
    FROM ACCOUNTS
    WHERE ID = :ACCOUNT-ID
END-EXEC.
```

### SQL Error Handling

```cobol
EXEC SQL
    INSERT INTO CUSTOMER VALUES (:WS-CUSTOMER)
END-EXEC.

IF SQLCODE NOT = 0
    DISPLAY "SQL Error: " SQLCODE
END-IF
```

## Mainframe Integration

### Compilation COBOL

**Compilation en mode FIXED (strict) :**

```jcl
//COMPILE  JOB (ACCT),'COMPILE COBOL'
//STEP1    EXEC PGM=IGYCRCTL,PARM='APOST'
//STEPLIB  DD DSN=IGY.SIGYCOMP,DISP=SHR
//SYSIN    DD DSN=MY.COBOL.SOURCE(PROG),DISP=SHR
//SYSPRINT DD SYSOUT=*
//SYSLIN   DD DSN=MY.OBJECT(PROG),DISP=(NEW,CATLG)
```

**Compilation en mode FREE :**

```jcl
//COMPILE  JOB (ACCT),'COMPILE FREE FORMAT'
//STEP1    EXEC PGM=IGYCRCTL,PARM='APOST,SOURCE(FREE)'
//STEPLIB  DD DSN=IGY.SIGYCOMP,DISP=SHR
//SYSIN    DD DSN=MY.COBOL.SOURCE(PROG),DISP=SHR
//SYSPRINT DD SYSOUT=*
//SYSLIN   DD DSN=MY.OBJECT(PROG),DISP=(NEW,CATLG)
```

### Options de Compilation Communes

| Option          | Description                                    |
| --------------- | ---------------------------------------------- |
| `SOURCE(FIXED)` | Mode colonnes traditionnelles (défaut)         |
| `SOURCE(FREE)`  | Mode format libre                              |
| `APOST`         | Utilise apostrophes pour délimiter les chaînes |
| `QUOTE`         | Utilise guillemets pour délimiter les chaînes  |
| `NOLIST`        | Supprime le listing source                     |
| `XREF`          | Génère une table de références croisées        |
| `MAP`           | Génère une carte mémoire                       |

### JCL d'Exécution

```jcl
//MYJOB    JOB (ACCT),'RUN COBOL PROGRAM'
//STEP01   EXEC PGM=COBOLPGM
//STEPLIB  DD DSN=MY.LOAD.LIB,DISP=SHR
//SYSPRINT DD SYSOUT=*
//SYSOUT   DD SYSOUT=*
//SYSIN    DD *
données d'entrée
/*
```

### Compilation et Link-Edit Complets

```jcl
//COMPLINK JOB (ACCT),'COMPILE AND LINK'
//COMPILE  EXEC PGM=IGYCRCTL,PARM='APOST,XREF'
//STEPLIB  DD DSN=IGY.SIGYCOMP,DISP=SHR
//SYSIN    DD DSN=MY.COBOL.SOURCE(PROG),DISP=SHR
//SYSPRINT DD SYSOUT=*
//SYSLIN   DD DSN=&&OBJECT,DISP=(NEW,PASS),UNIT=SYSDA
//
//LINK     EXEC PGM=IEWL,PARM='MAP,LIST,XREF'
//SYSLIB   DD DSN=CEE.SCEELKED,DISP=SHR
//SYSLIN   DD DSN=&&OBJECT,DISP=(OLD,DELETE)
//SYSLMOD  DD DSN=MY.LOAD.LIB(PROG),DISP=SHR
//SYSPRINT DD SYSOUT=*
```

## Common Functions & Intrinsics

### String Functions

```cobol
FUNCTION UPPER-CASE(NAME)
FUNCTION LOWER-CASE(TEXT)
FUNCTION LENGTH(STRING-VAR)
FUNCTION TRIM(INPUT-STRING)
```

### Numeric Functions

```cobol
FUNCTION NUMVAL(NUMERIC-STRING)
FUNCTION INTEGER(DECIMAL-NUMBER)
FUNCTION MOD(DIVIDEND, DIVISOR)
```

### Date Functions

```cobol
FUNCTION CURRENT-DATE
FUNCTION INTEGER-OF-DATE(DATE-VAR)
FUNCTION DATE-OF-INTEGER(INTEGER-VAR)
```
