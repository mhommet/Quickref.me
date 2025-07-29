---
title: JCL
date: 2024-01-15 10:00:00
background: bg-[#0f4c81]
tags:
  - IBM
  - z/OS
  - Mainframe
categories:
  - Programming
intro: |
  A quick reference cheat sheet for Job Control Language (JCL) that includes syntax, statements, and examples for z/OS systems.
plugins:
  - copyCode
---

## Introduction to JCL

### What is JCL?

Job Control Language (JCL) is a set of statements that you code to tell the z/OS operating system about the work you want it to perform. JCL statements tell z/OS where to find the appropriate input, how to process that input, and what to do with the resulting output.

### Basic JCL Structure

```jcl
//JOBNAME  JOB  (accounting),programmer,CLASS=A
//STEP1    EXEC PGM=program-name
//SYSOUT   DD   SYSOUT=*
//SYSIN    DD   *
input data
/*
```

All jobs use three main types of JCL statements:

- One **JOB** statement to identify the unit of work
- One or more **EXEC** statements for job steps
- One or more **DD** statements to identify data sets

## JOB Statement

### Basic JOB Statement

```jcl
//JOBNAME  JOB  (accounting-info),programmer-name
```

The JOB statement is the first control statement in a job and marks the beginning of a job.

### JOB Statement Parameters

```jcl
//MYJOB001 JOB  (12345,DEV),'JOHN DOE',
//              CLASS=A,
//              MSGCLASS=H,
//              MSGLEVEL=(1,1),
//              TIME=(0,30),
//              REGION=4M
```

### Common JOB Parameters

| Parameter  | Description              | Example          |
| ---------- | ------------------------ | ---------------- |
| `CLASS`    | Job class for scheduling | `CLASS=A`        |
| `MSGCLASS` | Output message class     | `MSGCLASS=H`     |
| `MSGLEVEL` | Message level control    | `MSGLEVEL=(1,1)` |
| `TIME`     | Maximum execution time   | `TIME=(0,30)`    |
| `REGION`   | Memory allocation        | `REGION=4M`      |
| `NOTIFY`   | User to notify           | `NOTIFY=&SYSUID` |

## EXEC Statement

### Program Execution

```jcl
//STEP1    EXEC PGM=IEFBR14
//STEP2    EXEC PGM=SORT
//STEP3    EXEC PGM=IDCAMS
```

The EXEC statement marks the beginning of a step and specifies the program to run.

### Procedure Execution

```jcl
//STEP1    EXEC PROC=MYPROC
//STEP2    EXEC COBCLG
```

### EXEC Parameters

```jcl
//STEP1    EXEC PGM=MYPROG,
//              PARM='OPTION1,OPTION2',
//              TIME=(0,10),
//              REGION=2M
```

### Common EXEC Parameters

| Parameter | Description                  | Example             |
| --------- | ---------------------------- | ------------------- |
| `PARM`    | Parameters passed to program | `PARM='LIST,NOMAP'` |
| `TIME`    | Step time limit              | `TIME=(0,5)`        |
| `REGION`  | Step memory allocation       | `REGION=1M`         |
| `COND`    | Conditional execution        | `COND=(4,LT)`       |

## DD Statement

### Basic DD Statement

```jcl
//SYSOUT   DD   SYSOUT=*
//SYSIN    DD   *
//SYSPRINT DD   SYSOUT=*
```

Data Definition (DD) statements define the data sets that a program uses.

### Sequential Data Set

```jcl
//INPUT    DD   DSN=MY.INPUT.FILE,
//              DISP=(OLD,KEEP,DELETE),
//              UNIT=SYSDA,
//              VOL=SER=DISK01
```

### Partitioned Data Set (PDS)

```jcl
//SYSPUNCH DD   DSN=MY.SOURCE.LIB(MEMBER),
//              DISP=(OLD,KEEP,DELETE)
```

### Creating New Data Sets

```jcl
//OUTPUT   DD   DSN=MY.OUTPUT.FILE,
//              DISP=(NEW,CATLG,DELETE),
//              UNIT=SYSDA,
//              SPACE=(CYL,(5,2)),
//              DCB=(RECFM=FB,LRECL=80,BLKSIZE=3200)
```

## DD Statement Parameters {.row-span-2}

### DISP Parameter

```jcl
DISP=(status,normal-disposition,abnormal-disposition)
```

| Status | Description               |
| ------ | ------------------------- |
| `NEW`  | Create new data set       |
| `OLD`  | Exclusive use of existing |
| `SHR`  | Shared use of existing    |
| `MOD`  | Modify existing or create |

| Disposition | Description        |
| ----------- | ------------------ |
| `KEEP`      | Keep data set      |
| `DELETE`    | Delete data set    |
| `CATLG`     | Catalog data set   |
| `UNCATLG`   | Uncatalog data set |
| `PASS`      | Pass to next step  |

### SPACE Parameter

```jcl
SPACE=(unit,(primary,secondary,directory),RLSE)
```

Examples:

```jcl
SPACE=(CYL,(10,5))      // Cylinders
SPACE=(TRK,(50,10))     // Tracks
SPACE=(80,(100,20,10))  // Records
```

### DCB Parameter

```jcl
DCB=(RECFM=recfm,LRECL=length,BLKSIZE=size)
```

Examples:

```jcl
DCB=(RECFM=FB,LRECL=80,BLKSIZE=3200)   // Fixed blocked
DCB=(RECFM=VB,LRECL=255,BLKSIZE=6144)  // Variable blocked
```

## Special DD Statements

### Inline Data

```jcl
//SYSIN    DD   *
This is inline data
that goes directly
into the program
/*
```

### Dummy Data Set

```jcl
//DUMMY    DD   DUMMY
```

### System Output

```jcl
//SYSOUT   DD   SYSOUT=*        // Default output class
//REPORT   DD   SYSOUT=A        // Specific output class
//LIST     DD   SYSOUT=(A,HOLD) // Hold output
```

### Concatenation

```jcl
//INPUT    DD   DSN=FILE1,DISP=SHR
//         DD   DSN=FILE2,DISP=SHR
//         DD   DSN=FILE3,DISP=SHR
```

## Conditional Processing

### COND Parameter

```jcl
//STEP1    EXEC PGM=PROG1
//STEP2    EXEC PGM=PROG2,COND=(4,LT,STEP1)
//STEP3    EXEC PGM=PROG3,COND=((4,LT,STEP1),(8,LE,STEP2))
```

### IF/THEN/ELSE/ENDIF

```jcl
//         IF (STEP1.RC = 0) THEN
//STEP2    EXEC PGM=PROG2
//         ELSE
//STEP3    EXEC PGM=PROG3
//         ENDIF
```

## Procedures

### Defining a Procedure

```jcl
//MYPROC   PROC MEMBER=TEMP
//STEP1    EXEC PGM=IEBGENER
//SYSUT1   DD   DSN=&MEMBER,DISP=SHR
//SYSUT2   DD   SYSOUT=*
//SYSIN    DD   DUMMY
//SYSPRINT DD   SYSOUT=*
//         PEND
```

### Calling a Procedure

```jcl
//STEP1    EXEC MYPROC,MEMBER=MYFILE
```

### Overriding Procedure Parameters

```jcl
//STEP1    EXEC MYPROC
//STEP1.SYSUT2 DD SYSOUT=A
```

## Common Utilities

### IEFBR14 (Do Nothing)

```jcl
//STEP1    EXEC PGM=IEFBR14
//NEWFILE  DD   DSN=MY.NEW.FILE,
//              DISP=(NEW,CATLG,DELETE),
//              UNIT=SYSDA,
//              SPACE=(CYL,(1,1))
```

### IEBGENER (Copy Utility)

```jcl
//COPY     EXEC PGM=IEBGENER
//SYSUT1   DD   DSN=INPUT.FILE,DISP=SHR
//SYSUT2   DD   DSN=OUTPUT.FILE,
//              DISP=(NEW,CATLG,DELETE),
//              LIKE=INPUT.FILE
//SYSIN    DD   DUMMY
//SYSPRINT DD   SYSOUT=*
```

### SORT

```jcl
//SORT     EXEC PGM=SORT
//SYSOUT   DD   SYSOUT=*
//SORTIN   DD   DSN=INPUT.FILE,DISP=SHR
//SORTOUT  DD   DSN=SORTED.FILE,
//              DISP=(NEW,CATLG,DELETE),
//              LIKE=INPUT.FILE
//SYSIN    DD   *
  SORT FIELDS=(1,10,CH,A)
/*
```

### IDCAMS (Access Method Services)

```jcl
//DEFINE   EXEC PGM=IDCAMS
//SYSPRINT DD   SYSOUT=*
//SYSIN    DD   *
  DEFINE CLUSTER(NAME(MY.VSAM.FILE) -
                 VOLUMES(DISK01) -
                 RECORDS(1000) -
                 RECORDSIZE(80 80))
/*
```

## JCL Symbols and Variables

### System Symbols

```jcl
//MYJOB    JOB  ,'&SYSUID',NOTIFY=&SYSUID
//STEP1    EXEC PGM=MYPROG
//OUTPUT   DD   DSN=&SYSUID..OUTPUT.&SYSDATE,
//              DISP=(NEW,CATLG,DELETE)
```

### SET Statement

```jcl
//         SET  MYLIB=PROD.LIBRARY
//         SET  MEMBER=PROGRAM1
//STEP1    EXEC PGM=&MEMBER
//STEPLIB  DD   DSN=&MYLIB,DISP=SHR
```

### Symbolic Parameters in Procedures

```jcl
//MYPROC   PROC LIB=DEFAULT.LIB,MEM=TEMP
//STEP1    EXEC PGM=&MEM
//STEPLIB  DD   DSN=&LIB,DISP=SHR
//         PEND
```

## Return Codes and Error Handling

### Common Return Codes

| Return Code | Meaning              |
| ----------- | -------------------- |
| 0           | Successful execution |
| 4           | Warning condition    |
| 8           | Error condition      |
| 12          | Severe error         |
| 16          | Terminal error       |

### COND Operators

| Operator | Meaning               |
| -------- | --------------------- |
| `GT`     | Greater than          |
| `GE`     | Greater than or equal |
| `EQ`     | Equal to              |
| `LE`     | Less than or equal    |
| `LT`     | Less than             |
| `NE`     | Not equal to          |

## See Also

- [IBM z/OS Basic Skills Documentation](https://www.ibm.com/docs/hr/zos-basic-skills?topic=collection-basic-jcl-concepts) _(ibm.com)_
- [IBM z/OS JCL Reference](https://www.ibm.com/docs/en/zos) _(ibm.com)_
