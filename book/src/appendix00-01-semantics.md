# Semantics

A formalization of the semantics for type checking and normalizing Pikelet.

## Contents

- [Introduction](#introduction)
  - [Notation](#notation)
- [Syntax](#syntax)
  - [Raw terms](#raw-terms)
  - [Terms](#terms)
  - [Values](#values)
- [Semantics](#semantics)
  - [Normalization](#normalization)
  - [Type checking](#type-checking)
  - [Type inference](#type-inference)
- [Examples](#examples)

## Introduction

> **Note:**
> This document is intended for those who are interested in looking deeper into the formal foundations of Pikelet.
> You _don't_ need to understand this for general use of Pikelet, so feel free to skip this document if that is easier.
> We will however make an effort to explain some of the notation we use here, and point to resources that might help if this piques your curiosity!

### Notation

TODO: describe BNF syntax and natural deduction here

## Syntax

### Raw terms

### Terms

\\[
% Core metavariables
\\newcommand{\texpr}{t}
\\newcommand{\ttype}{T}
\\
% Type constructors
\\newcommand{\Type}{\mathsf{Type}}
\\newcommand{\Arrow}[2]{ #1 \rightarrow #2 }
\\
\begin{array}{rrll}
    \texpr,\ttype   & ::= & x                               & \text{variables} \\\\
                    &   | & \Type_i                         & \text{universe of types ($i \in \mathbb{N}$)} \\\\
                    &   | & \texpr : \ttype                 & \text{term annotated with a type} \\\\
                    &   | & \Arrow{(x:\ttype_1)}{\ttype_2}  & \text{dependent function type} \\\\
                    &   | & \lambda x:\ttype.\texpr         & \text{functions} \\\\
                    &   | & \texpr_1 ~ \texpr_2             & \text{function application} \\\\
    \\\\
\end{array}
\\]


### Values

## Semantics

### Normalization

### Type checking

### Type inference

## Examples

TODO: show some examples of well typed terms with their proof trees here
