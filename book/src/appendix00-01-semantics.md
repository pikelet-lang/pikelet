# Semantics

A formalization of the semantics for type checking and normalizing Pikelet.

## Contents

- [Introduction](#introduction)
  - [Notation](#notation)
- [Syntax](#syntax)
  - [Raw terms](#raw-terms)
  - [Terms](#terms)
  - [Values](#values)
  - [Contexts](#contexts)
- [Semantics](#semantics)
  - [Normalization](#normalization)
  - [Type checking](#type-checking)
  - [Type inference](#type-inference)

## Introduction

At its core, Pikelet is a simple dependently typed lambda calculus with a
stratified universe hierarchy.

> **Note:**
> This document is intended for those who are interested in looking deeper into the formal foundations of Pikelet.
> You _don't_ need to understand this for general use of Pikelet, so feel free to skip this document if that is easier.
> We will however make an effort to explain some of the notation we use here, and point to resources that might help if this piques your curiosity!

### Notation

TODO: describe BNF syntax and natural deduction here

## Syntax

### Raw terms

\\[
\\newcommand{\rule}[3]{ \dfrac{ ~~#2~~ }{ ~~#3~~ } & \Tiny{\text{(#1)}} }
\\
% Judgements
\\newcommand{\eval}[3]{ #1 \vdash #2 \Rightarrow #3 }
\\newcommand{\check}[4]{ #1 \vdash #2 \uparrow #3 \rhd #4 }
\\newcommand{\infer}[4]{ #1 \vdash #2 \downarrow #3 \rhd #4 }
\\
% Metavariables
\\newcommand{\rexpr}{r}
\\newcommand{\rtype}{R}
\\
\\newcommand{\texpr}{t}
\\newcommand{\ttype}{T}
\\
\\newcommand{\vexpr}{v}
\\newcommand{\vtype}{V}
\\newcommand{\wexpr}{w}
\\newcommand{\wtype}{W}
\\newcommand{\nexpr}{n}
\\newcommand{\ntype}{N}
\\
% Type constructors
\\newcommand{\Type}{\mathsf{Type}}
\\newcommand{\Arrow}[2]{ #1 \rightarrow #2 }
\\
\begin{array}{rrll}
    \rexpr,\rtype   & ::= & x                               & \text{variables} \\\\
                    &   | & \Type_i                         & \text{universe of types ($i \in \mathbb{N}$)} \\\\
                    &   | & ?                               & \text{holes} \\\\
                    &   | & \rexpr : \rtype                 & \text{term annotated with a type} \\\\
                    &   | & \Arrow{(x:\rtype_1)}{\rtype_2}  & \text{dependent function type} \\\\
                    &   | & \lambda x:\rtype.\rexpr         & \text{functions} \\\\
                    &   | & \rexpr_1 ~ \rexpr_2             & \text{function application} \\\\
    \\\\
\end{array}
\\]

\\[
\begin{array}{lrll}
    \Arrow{\rtype_1}{\rtype_2} & := & \Arrow{(x:\rtype_1)}{\rtype_2}  & \text{non-dependent function types} \\\\
    \lambda x.\rexpr           & := & \lambda x:?.\rexpr              & \text{functions (without an annotation)} \\\\
\end{array}
\\]

### Terms

The core term syntax skips holes, ensuring that everything is fully elaborated:

\\[
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

In order to make it clear what is 'stuck' and what still needs to be evaluated,
we separate our syntax into weak head normal forms (\\(\wexpr\\)),
and neutral terms (\\(\nexpr\\)):

\\[
\begin{array}{rrll}
    \vexpr,\vtype   & ::= & \wexpr                          & \text{weak head normal forms} \\\\
                    &   | & \nexpr                          & \text{neutral terms} \\\\
    \\\\
    \nexpr,\ntype   & ::= & x                               & \text{variables} \\\\
                    &   | & \nexpr ~ \texpr                 & \text{function application} \\\\
    \\\\
    \wexpr,\wtype   & ::= & \Type_i                         & \text{universe of types ($i \in \mathbb{N}$)} \\\\
                    &   | & \Arrow{(x:\vtype_1)}{\vtype_2}  & \text{dependent function type} \\\\
                    &   | & \lambda x:\vtype.\vexpr         & \text{functions} \\\\
    \\\\
\end{array}
\\]

Note that function application is stuck,
forming a linked list of terms bottoming out with a variable \\(x\\).
We require neutral terms to be evaluated in a context in order to figure out  whether the base variable is a function or not,
and if so proceed to reduce these applications to their weak head normal forms via subsititution.

### Contexts

As we typecheck terms, we'll be passing over bindings like lambdas and pi types.
Contexts allow us to keep track of the bound parameters,
even though we don't know the exact values these will eventually take during normalization.

\\[
\begin{array}{rrll}
    \Gamma  & ::= & \epsilon                    & \text{the empty context} \\\\
            &   | & \Gamma,x:\vtype             & \text{context extended with a type annotation} \\\\
            &   | & \Gamma,x:\vtype=\texpr      & \text{context extended with a definition} \\\\
\end{array}
\\]

## Semantics

We'll be define a bidirectional type checking algorithm.
To that end, the next sections will define the following judgements:

| name                              | notation                                             | inputs                                   | outputs                    |
|-----------------------------------|------------------------------------------------------|------------------------------------------|----------------------------|
| [normalization](#normalization)   | \\(\eval{ \Gamma }{ \texpr }{ \vexpr }\\)            | \\(\Gamma\\), \\(\rexpr\\)               | \\(\vexpr\\)               |
| [type checking](#type-checking)   | \\(\check{ \Gamma }{ \rexpr }{ \vtype }{ \texpr }\\) | \\(\Gamma\\), \\(\rexpr\\), \\(\vtype\\) | \\(\texpr\\)               |
| [type inference](#type-inference) | \\(\infer{ \Gamma }{ \rexpr }{ \vtype }{ \texpr }\\) | \\(\Gamma\\), \\(\rexpr\\)               | \\(\vtype\\), \\(\texpr\\) |

### Normalization

\\[
\boxed{
    \eval{ \Gamma }{ \texpr }{ \vexpr }
}
\\\\[2em]
\begin{array}{cl}
    \rule{E-ANN}{
        \eval{ \Gamma }{ \texpr }{ \vexpr }
    }{
        \eval{ \Gamma }{ \texpr:\ttype }{ \vexpr }
    }
    \\\\[2em]
    \rule{E-TYPE}{}{
        \eval{ \Gamma }{ \Type_i }{ \Type_i }
    }
    \\\\[2em]
\end{array}
\\]

### Type checking

\\[
\boxed{
    \check{ \Gamma }{ \rexpr }{ \vtype }{ \texpr }
}
\\]

### Type inference

\\[
\boxed{
    \infer{ \Gamma }{ \rexpr }{ \vtype }{ \texpr }
}
\\]
