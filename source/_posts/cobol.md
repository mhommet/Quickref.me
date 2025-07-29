---
title: COBOL
date: 2025-07-29 10:32:03
background: bg-indigo-900
tags:
categories:
  - Programming
intro: |
  A quick reference guide to COBOL syntax and concepts, ideal for beginners or those returning to mainframe development.
plugins:
  - copyCode
icon: https://img.icons8.com/external-flatart-icons-outline-flatarticons/64/33b1ff/external-mainframe-technology-flatart-icons-outline-flatarticons.png
---

## 📂 COBOL Program Structure

### 📂 COBOL Program Example

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

## 📊 Variables and Simple Types

### 🧠 Most Used COBOL Types

| Syntax         | Data Type                    | Example                |
| -------------- | ---------------------------- | ---------------------- |
| `PIC 9(n)`     | Positive integer             | `PIC 9(5)` → 00042     |
| `PIC S9(n)`    | Signed integer               | `PIC S9(5)` → -00123   |
| `PIC 9(n)V99`  | Decimal number (virtual)     | `PIC 9(3)V99` → 123.45 |
| `PIC A(n)`     | Alphabetic characters        | `PIC A(10)` → "MILAN"  |
| `PIC X(n)`     | Free alphanumeric string     | `PIC X(10)` → "ABC123" |
| `PIC +ZZZ9.99` | Display format with sign    | `+0123.45`, `-0098.60` |

### 💡 Example

```cobol
01 AGE         PIC 99.        *> 2-digit integer
01 BALANCE     PIC S9(5)V99.  *> Number with decimals (signed)
01 NAME        PIC A(30).     *> Alpha string
```

### ✉ ACCEPT & DISPLAY (input/output)

```cobol
DISPLAY "What is your name?"
ACCEPT NAME
DISPLAY "Hello " NAME
```

### ➕ COMPUTE, MOVE, IF

```cobol
COMPUTE VAT = PRICE * RATE / 100
IF AGE > 18
    MOVE "adult" TO STATUS
ELSE
    MOVE "minor" TO STATUS
END-IF
```

### 🛠️ PERFORM (loops)

```cobol
PERFORM VARYING I FROM 1 BY 1 UNTIL I > 5
    DISPLAY "Round: " I
END-PERFORM
```

### 🧱 Nested Structures (GROUP LEVEL)

```cobol
01 CLIENT.
   05 LAST-NAME   PIC A(30).
   05 FIRST-NAME  PIC A(30).
   05 AGE         PIC 99.
```

### 📄 Arrays (OCCURS)

```cobol
01 ACCOUNTS.
   05 ACCOUNT OCCURS 5 TIMES.
      10 ID          PIC 9(5).
      10 BALANCE     PIC 9(7)V99.
```

### 🌐 File Reading/Display (intro)

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

### 📅 EVALUATE (switch-case)

```cobol
EVALUATE CHOICE
    WHEN 1 DISPLAY "Option 1"
    WHEN 2 DISPLAY "Option 2"
    WHEN OTHER DISPLAY "Invalid"
END-EVALUATE
```

### 📊 DB2 (embedded SQL intro)

```cobol
EXEC SQL
    SELECT BALANCE INTO :ACCOUNT-BALANCE
    FROM ACCOUNTS
    WHERE ID = :ACCOUNT-ID
END-EXEC.
```

> Uses `SQLCA`, `:VARIABLE`, and a DB2 precompiler to work.

### ⛏ JCL (Job Control Language)

```jcl
//MYJOB    JOB (ACCT),'MY JOB',CLASS=A
//STEP01   EXEC PGM=CALCULS
//SYSPRINT DD SYSOUT=*
```

> Used to execute a COBOL program on a mainframe.