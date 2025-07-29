---
title: DB2
date: 2024-01-15 12:00:00
background: bg-[#1f4e79]
tags:
  - SQL
  - Database
  - Mainframe
  - z/OS
categories:
  - Database
intro: |
  A comprehensive reference guide for DB2 database including SQL statements, data types, functions, and mainframe-specific features.
plugins:
  - copyCode
---

## Introduction to DB2

### What is DB2?

DB2 is IBM's relational database management system (RDBMS) that runs on various platforms including z/OS mainframes. It supports SQL (Structured Query Language) for data manipulation and control.

### SQL Statement Types

| Type | Purpose | Commands |
|------|---------|----------|
| **DDL** | Data Definition Language | `CREATE`, `ALTER`, `DROP` |
| **DML** | Data Manipulation Language | `SELECT`, `INSERT`, `UPDATE`, `DELETE` |
| **DCL** | Data Control Language | `GRANT`, `REVOKE` |
| **TCL** | Transaction Control Language | `COMMIT`, `ROLLBACK` |

## Basic SQL Syntax

### SELECT Statement

```sql
SELECT column1, column2, ...
FROM table_name
WHERE condition
GROUP BY column
HAVING condition
ORDER BY column ASC|DESC;
```

### Basic Examples

```sql
-- Select all columns
SELECT * FROM EMPLOYEE;

-- Select specific columns
SELECT EMP_ID, EMP_NAME, SALARY FROM EMPLOYEE;

-- With WHERE condition
SELECT EMP_NAME, SALARY 
FROM EMPLOYEE 
WHERE DEPARTMENT = 'SALES';
```

## DDL - Data Definition Language

### CREATE TABLE

```sql
CREATE TABLE EMPLOYEE (
    EMP_ID INTEGER NOT NULL PRIMARY KEY,
    EMP_NAME VARCHAR(50) NOT NULL,
    DEPARTMENT VARCHAR(20),
    SALARY DECIMAL(10,2),
    HIRE_DATE DATE,
    STATUS CHAR(1) DEFAULT 'A'
);
```

### ALTER TABLE

```sql
-- Add column
ALTER TABLE EMPLOYEE 
ADD COLUMN EMAIL VARCHAR(100);

-- Modify column
ALTER TABLE EMPLOYEE 
ALTER COLUMN SALARY SET DATA TYPE DECIMAL(12,2);

-- Drop column
ALTER TABLE EMPLOYEE 
DROP COLUMN EMAIL;
```

### CREATE INDEX

```sql
CREATE INDEX IDX_EMP_DEPT 
ON EMPLOYEE (DEPARTMENT);

CREATE UNIQUE INDEX IDX_EMP_EMAIL 
ON EMPLOYEE (EMAIL);
```

### DROP Statements

```sql
DROP TABLE EMPLOYEE;
DROP INDEX IDX_EMP_DEPT;
DROP VIEW V_ACTIVE_EMPLOYEES;
```

## DML - Data Manipulation Language

### INSERT Statement

```sql
-- Insert single row
INSERT INTO EMPLOYEE 
VALUES (101, 'John Smith', 'SALES', 50000.00, '2024-01-15', 'A');

-- Insert with specific columns
INSERT INTO EMPLOYEE (EMP_ID, EMP_NAME, DEPARTMENT, SALARY)
VALUES (102, 'Jane Doe', 'IT', 65000.00);

-- Insert multiple rows
INSERT INTO EMPLOYEE VALUES 
    (103, 'Bob Johnson', 'HR', 45000.00, '2024-01-10', 'A'),
    (104, 'Alice Brown', 'IT', 70000.00, '2024-01-12', 'A');
```

### UPDATE Statement

```sql
-- Update single column
UPDATE EMPLOYEE 
SET SALARY = 55000.00 
WHERE EMP_ID = 101;

-- Update multiple columns
UPDATE EMPLOYEE 
SET SALARY = SALARY * 1.10, STATUS = 'P'
WHERE DEPARTMENT = 'SALES';

-- Update with subquery
UPDATE EMPLOYEE 
SET SALARY = (SELECT AVG(SALARY) FROM EMPLOYEE WHERE DEPARTMENT = 'IT')
WHERE EMP_ID = 101;
```

### DELETE Statement

```sql
-- Delete specific rows
DELETE FROM EMPLOYEE 
WHERE STATUS = 'I';

-- Delete with subquery
DELETE FROM EMPLOYEE 
WHERE EMP_ID IN (SELECT EMP_ID FROM TEMP_LAYOFFS);
```

## WHERE Clause and Operators

### Comparison Operators

```sql
-- Basic comparisons
SELECT * FROM EMPLOYEE WHERE SALARY > 50000;
SELECT * FROM EMPLOYEE WHERE DEPARTMENT = 'SALES';
SELECT * FROM EMPLOYEE WHERE HIRE_DATE >= '2024-01-01';

-- BETWEEN
SELECT * FROM EMPLOYEE WHERE SALARY BETWEEN 40000 AND 60000;

-- IN
SELECT * FROM EMPLOYEE WHERE DEPARTMENT IN ('SALES', 'IT', 'HR');

-- LIKE with wildcards
SELECT * FROM EMPLOYEE WHERE EMP_NAME LIKE 'J%';     -- Starts with J
SELECT * FROM EMPLOYEE WHERE EMP_NAME LIKE '%son';   -- Ends with son
SELECT * FROM EMPLOYEE WHERE EMP_NAME LIKE '%a%';    -- Contains a
```

### Logical Operators

```sql
-- AND, OR, NOT
SELECT * FROM EMPLOYEE 
WHERE DEPARTMENT = 'SALES' AND SALARY > 50000;

SELECT * FROM EMPLOYEE 
WHERE DEPARTMENT = 'SALES' OR DEPARTMENT = 'IT';

SELECT * FROM EMPLOYEE 
WHERE NOT DEPARTMENT = 'HR';

-- IS NULL / IS NOT NULL
SELECT * FROM EMPLOYEE WHERE EMAIL IS NULL;
SELECT * FROM EMPLOYEE WHERE EMAIL IS NOT NULL;
```

## DB2 Data Types {.row-span-2}

### Numeric Data Types

| Type | Description | Example |
|------|-------------|---------|
| `INTEGER` | 32-bit integer | `EMP_ID INTEGER` |
| `SMALLINT` | 16-bit integer | `AGE SMALLINT` |
| `BIGINT` | 64-bit integer | `BIG_NUMBER BIGINT` |
| `DECIMAL(p,s)` | Fixed decimal | `SALARY DECIMAL(10,2)` |
| `NUMERIC(p,s)` | Same as DECIMAL | `PRICE NUMERIC(8,2)` |
| `REAL` | Single precision float | `RATE REAL` |
| `DOUBLE` | Double precision float | `FACTOR DOUBLE` |

### Character Data Types

| Type | Description | Example |
|------|-------------|---------|
| `CHAR(n)` | Fixed length | `STATUS CHAR(1)` |
| `VARCHAR(n)` | Variable length | `NAME VARCHAR(50)` |
| `CLOB` | Character large object | `DESCRIPTION CLOB` |

### Date/Time Data Types

| Type | Description | Example |
|------|-------------|---------|
| `DATE` | Date only | `HIRE_DATE DATE` |
| `TIME` | Time only | `START_TIME TIME` |
| `TIMESTAMP` | Date and time | `CREATED_AT TIMESTAMP` |

## Built-in Functions

### Aggregate Functions

```sql
-- COUNT
SELECT COUNT(*) FROM EMPLOYEE;
SELECT COUNT(DISTINCT DEPARTMENT) FROM EMPLOYEE;

-- SUM, AVG, MIN, MAX
SELECT 
    SUM(SALARY) AS TOTAL_SALARY,
    AVG(SALARY) AS AVG_SALARY,
    MIN(SALARY) AS MIN_SALARY,
    MAX(SALARY) AS MAX_SALARY
FROM EMPLOYEE;
```

### String Functions

```sql
-- SUBSTR
SELECT SUBSTR(EMP_NAME, 1, 10) FROM EMPLOYEE;

-- LENGTH
SELECT EMP_NAME, LENGTH(EMP_NAME) FROM EMPLOYEE;

-- UPPER, LOWER
SELECT UPPER(EMP_NAME), LOWER(DEPARTMENT) FROM EMPLOYEE;

-- TRIM
SELECT TRIM(EMP_NAME) FROM EMPLOYEE;

-- CONCAT or ||
SELECT EMP_NAME || ' - ' || DEPARTMENT FROM EMPLOYEE;
```

### Date Functions

```sql
-- Current date/time
SELECT CURRENT DATE, CURRENT TIME, CURRENT TIMESTAMP;

-- Date arithmetic
SELECT EMP_NAME, HIRE_DATE, 
       (CURRENT DATE - HIRE_DATE) DAYS AS DAYS_EMPLOYED
FROM EMPLOYEE;

-- YEAR, MONTH, DAY
SELECT YEAR(HIRE_DATE), MONTH(HIRE_DATE), DAY(HIRE_DATE)
FROM EMPLOYEE;
```

### Numeric Functions

```sql
-- ROUND, FLOOR, CEILING
SELECT SALARY, 
       ROUND(SALARY, 0) AS ROUNDED,
       FLOOR(SALARY) AS FLOORED,
       CEILING(SALARY) AS CEILED
FROM EMPLOYEE;

-- ABS
SELECT ABS(-100);  -- Returns 100
```

## JOIN Operations

### INNER JOIN

```sql
SELECT e.EMP_NAME, d.DEPT_NAME
FROM EMPLOYEE e
INNER JOIN DEPARTMENT d ON e.DEPT_ID = d.DEPT_ID;
```

### LEFT OUTER JOIN

```sql
SELECT e.EMP_NAME, d.DEPT_NAME
FROM EMPLOYEE e
LEFT OUTER JOIN DEPARTMENT d ON e.DEPT_ID = d.DEPT_ID;
```

### RIGHT OUTER JOIN

```sql
SELECT e.EMP_NAME, d.DEPT_NAME
FROM EMPLOYEE e
RIGHT OUTER JOIN DEPARTMENT d ON e.DEPT_ID = d.DEPT_ID;
```

### FULL OUTER JOIN

```sql
SELECT e.EMP_NAME, d.DEPT_NAME
FROM EMPLOYEE e
FULL OUTER JOIN DEPARTMENT d ON e.DEPT_ID = d.DEPT_ID;
```

## Subqueries

### Single Row Subquery

```sql
SELECT EMP_NAME, SALARY
FROM EMPLOYEE
WHERE SALARY > (SELECT AVG(SALARY) FROM EMPLOYEE);
```

### Multiple Row Subquery

```sql
-- EXISTS
SELECT EMP_NAME
FROM EMPLOYEE e
WHERE EXISTS (SELECT 1 FROM PROJECT p WHERE p.EMP_ID = e.EMP_ID);

-- IN
SELECT EMP_NAME
FROM EMPLOYEE
WHERE DEPT_ID IN (SELECT DEPT_ID FROM DEPARTMENT WHERE LOCATION = 'NEW YORK');
```

### Correlated Subquery

```sql
SELECT EMP_NAME, SALARY, DEPARTMENT
FROM EMPLOYEE e1
WHERE SALARY > (SELECT AVG(SALARY) 
                FROM EMPLOYEE e2 
                WHERE e2.DEPARTMENT = e1.DEPARTMENT);
```

## GROUP BY and HAVING

### GROUP BY

```sql
SELECT DEPARTMENT, COUNT(*) AS EMP_COUNT, AVG(SALARY) AS AVG_SALARY
FROM EMPLOYEE
GROUP BY DEPARTMENT;
```

### HAVING Clause

```sql
SELECT DEPARTMENT, COUNT(*) AS EMP_COUNT
FROM EMPLOYEE
GROUP BY DEPARTMENT
HAVING COUNT(*) > 5;
```

## CASE Statement

### Simple CASE

```sql
SELECT EMP_NAME, SALARY,
    CASE 
        WHEN SALARY < 40000 THEN 'Low'
        WHEN SALARY < 60000 THEN 'Medium'
        ELSE 'High'
    END AS SALARY_GRADE
FROM EMPLOYEE;
```

### Searched CASE

```sql
SELECT EMP_NAME,
    CASE DEPARTMENT
        WHEN 'SALES' THEN 'Revenue Generator'
        WHEN 'IT' THEN 'Technology'
        WHEN 'HR' THEN 'Support'
        ELSE 'Other'
    END AS DEPT_TYPE
FROM EMPLOYEE;
```

## Transaction Control

### COMMIT and ROLLBACK

```sql
-- Start transaction (implicit)
UPDATE EMPLOYEE SET SALARY = SALARY * 1.10 WHERE DEPARTMENT = 'SALES';

-- Commit changes
COMMIT;

-- Or rollback if needed
ROLLBACK;
```

### SAVEPOINT

```sql
-- Create savepoint
SAVEPOINT SP1;

UPDATE EMPLOYEE SET SALARY = SALARY * 1.05 WHERE DEPARTMENT = 'IT';

-- Rollback to savepoint
ROLLBACK TO SAVEPOINT SP1;
```

## DB2 for z/OS Specific Features

### DB2 System Tables

```sql
-- View table information
SELECT * FROM SYSIBM.SYSTABLES WHERE CREATOR = 'MYSCHEMA';

-- View column information
SELECT * FROM SYSIBM.SYSCOLUMNS WHERE TBNAME = 'EMPLOYEE';

-- View index information
SELECT * FROM SYSIBM.SYSINDEXES WHERE TBNAME = 'EMPLOYEE';
```

### EXPLAIN Statement

```sql
-- Explain query execution plan
EXPLAIN PLAN SET QUERYNO = 1 FOR
SELECT * FROM EMPLOYEE WHERE DEPARTMENT = 'SALES';

-- View explain results
SELECT * FROM PLAN_TABLE WHERE QUERYNO = 1;
```

### SPUFI (SQL Processor Using File Input)

```sql
-- Typical SPUFI commands
SELECT CURRENT SQLID;
SELECT CURRENT SERVER;
SELECT CURRENT TIMESTAMP;
```

## COBOL Integration

### Host Variables

```cobol
WORKING-STORAGE SECTION.
01  HOST-VARIABLES.
    05  HV-EMP-ID        PIC S9(9) COMP.
    05  HV-EMP-NAME      PIC X(50).
    05  HV-SALARY        PIC S9(7)V99 COMP-3.
    05  HV-SQLCODE       PIC S9(9) COMP.

PROCEDURE DIVISION.
    EXEC SQL
        SELECT EMP_NAME, SALARY
        INTO :HV-EMP-NAME, :HV-SALARY
        FROM EMPLOYEE
        WHERE EMP_ID = :HV-EMP-ID
    END-EXEC.
    
    IF SQLCODE = 0
        DISPLAY 'Employee found: ' HV-EMP-NAME
    END-IF.
```

### Cursor Processing

```cobol
EXEC SQL
    DECLARE EMP_CURSOR CURSOR FOR
    SELECT EMP_ID, EMP_NAME, SALARY
    FROM EMPLOYEE
    WHERE DEPARTMENT = 'SALES'
END-EXEC.

EXEC SQL OPEN EMP_CURSOR END-EXEC.

PERFORM UNTIL SQLCODE NOT = 0
    EXEC SQL
        FETCH EMP_CURSOR
        INTO :HV-EMP-ID, :HV-EMP-NAME, :HV-SALARY
    END-EXEC
    
    IF SQLCODE = 0
        DISPLAY HV-EMP-ID ' ' HV-EMP-NAME ' ' HV-SALARY
    END-IF
END-PERFORM.

EXEC SQL CLOSE EMP_CURSOR END-EXEC.
```

## Performance Tips

### Indexing Best Practices

```sql
-- Create selective indexes
CREATE INDEX IDX_EMP_DEPT_SALARY ON EMPLOYEE (DEPARTMENT, SALARY);

-- Use RUNSTATS to update statistics
RUNSTATS TABLE MYSCHEMA.EMPLOYEE;
```

### Query Optimization

```sql
-- Use FETCH FIRST for limiting results
SELECT * FROM EMPLOYEE 
WHERE DEPARTMENT = 'SALES'
ORDER BY SALARY DESC
FETCH FIRST 10 ROWS ONLY;

-- Use EXISTS instead of IN for better performance
SELECT EMP_NAME FROM EMPLOYEE e
WHERE EXISTS (SELECT 1 FROM PROJECT p WHERE p.EMP_ID = e.EMP_ID);
```

## Error Codes and Handling

### Common SQLCODE Values

| SQLCODE | Description |
|---------|-------------|
| 0 | Successful execution |
| 100 | No data found |
| -104 | Illegal symbol |
| -180 | Date/time value out of range |
| -204 | Object does not exist |
| -407 | NULL value in NOT NULL column |
| -803 | Duplicate key violation |

### SQLSTATE Values

```sql
-- Check SQLSTATE for standardized error codes
DECLARE SQLSTATE CHAR(5);

-- Common SQLSTATE values:
-- '00000' - Success
-- '02000' - No data found  
-- '23505' - Unique constraint violation
-- '42000' - Syntax error
```

## See Also

- [IBM DB2 for z/OS Documentation](https://www.ibm.com/docs/en/db2-for-zos) _(ibm.com)_
- [DB2 Tutorial](https://www.db2tutorial.com/) _(db2tutorial.com)_
- [IBM DB2 SQL Reference](https://www.ibm.com/docs/en/db2-for-zos/12?topic=reference-sql) _(ibm.com)_
