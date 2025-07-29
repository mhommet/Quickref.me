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

## 📂 Structure d'un programme COBOL

### 📂 Exemple de programme COBOL

```cobol
IDENTIFICATION DIVISION.
PROGRAM-ID. MonProgramme.

ENVIRONMENT DIVISION.

DATA DIVISION.
WORKING-STORAGE SECTION.
01 NOM     PIC A(30).

PROCEDURE DIVISION.
    DISPLAY "Bonjour" NOM
    STOP RUN.
```

## 📊 Variables et types simples

### 🧠 Les types COBOL les plus utilisés

| Syntaxe        | Type de données             | Exemple                |
| -------------- | --------------------------- | ---------------------- |
| `PIC 9(n)`     | Entier positif              | `PIC 9(5)` → 00042     |
| `PIC S9(n)`    | Entier signé                | `PIC S9(5)` → -00123   |
| `PIC 9(n)V99`  | Nombre décimal (virtuel)    | `PIC 9(3)V99` → 123.45 |
| `PIC A(n)`     | Caractères alphabétiques    | `PIC A(10)` → "MILAN"  |
| `PIC X(n)`     | Chaîne alphanumérique libre | `PIC X(10)` → "ABC123" |
| `PIC +ZZZ9.99` | Format affichage avec signe | `+0123.45`, `-0098.60` |

### 💡 Exemple

```cobol
01 AGE         PIC 99.        *> Entier sur 2 chiffres
01 SOLDE       PIC S9(5)V99.  *> Nombre avec décimales (signé)
01 NOM         PIC A(30).     *> Chaîne alpha
```

### ✉ ACCEPT & DISPLAY (entrée/sortie)

```cobol
DISPLAY "Quel est ton nom ?"
ACCEPT NOM
DISPLAY "Bonjour " NOM
```

### ➕ COMPUTE, MOVE, IF

```cobol
COMPUTE TVA = PRIXHT * TAUX / 100
IF AGE > 18
    MOVE "majeur" TO ETAT
ELSE
    MOVE "mineur" TO ETAT
END-IF
```

### 🛠️ PERFORM (boucles)

```cobol
PERFORM VARYING I FROM 1 BY 1 UNTIL I > 5
    DISPLAY "Tour : " I
END-PERFORM
```

### 🧱 Structures imbriquées (GROUP LEVEL)

```cobol
01 CLIENT.
   05 NOM     PIC A(30).
   05 PRENOM  PIC A(30).
   05 AGE     PIC 99.
```

### 📄 Tableaux (OCCURS)

```cobol
01 COMPTES.
   05 COMPTE OCCURS 5 TIMES.
      10 ID       PIC 9(5).
      10 SOLDE    PIC 9(7)V99.
```

### 🌐 Lecture/affichage de fichiers (intro)

```cobol
SELECT FICHIER ASSIGN TO 'data.txt'
   ORGANIZATION IS LINE SEQUENTIAL.

FD FICHIER.
01 LIGNE PIC X(80).

OPEN INPUT FICHIER
READ FICHIER
    AT END DISPLAY "Fin du fichier"
END-READ
CLOSE FICHIER
```

### 📅 EVALUATE (switch-case)

```cobol
EVALUATE CHOIX
    WHEN 1 DISPLAY "Option 1"
    WHEN 2 DISPLAY "Option 2"
    WHEN OTHER DISPLAY "Invalide"
END-EVALUATE
```

### 📊 DB2 (intro SQL embarqué)

```cobol
EXEC SQL
    SELECT SOLDE INTO :SOLDE-COMPTE
    FROM COMPTES
    WHERE ID = :ID-COMPTE
END-EXEC.
```

> Utilise `SQLCA`, `:VARIABLE`, et un précompilateur DB2 pour fonctionner.

### ⛏ JCL (Job Control Language)

```jcl
//MONJOB   JOB (ACCT),'MILAN JOB',CLASS=A
//STEP01   EXEC PGM=CALCULS
//SYSPRINT DD SYSOUT=*
```

> Utilisé pour exécuter un programme COBOL sur un mainframe.