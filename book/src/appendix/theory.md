# Theory

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
  - [Elaboration](#elaboration)
  - [Normalization](#normalization)
  - [Type checking](#type-checking)
  - [Type inference](#type-inference)

## Introduction

At its core, Pikelet is a dependently typed lambda calculus with a stratified
universe hierarchy.

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
\\DeclareMathOperator{\max}{max}
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
% Term and Type constructors
\\newcommand{\const}{c}
\\newcommand{\Type}{\mathsf{Type}}
\\newcommand{\Arrow}[2]{ #1 \rightarrow #2 }
\\newcommand{\Pi}[2]{ \Arrow{(#1)}{#2} }
\\newcommand{\lam}[2]{ \lambda #1 . #2 }
\\
\begin{array}{rrll}
    \rexpr,\rtype   & ::= & x                           & \text{variables} \\\\
                    &   | & \Type_i                     & \text{universe of types ($i \in \mathbb{N}$)} \\\\
                    &   | & ?                           & \text{holes} \\\\
                    &   | & \const                      & \text{constants} \\\\
                    &   | & \rexpr : \rtype             & \text{term annotated with a type} \\\\
                    &   | & \Pi{x:\rtype_1}{\rtype_2}   & \text{dependent function type} \\\\
                    &   | & \lam{x:\rtype}{\rexpr}      & \text{functions} \\\\
                    &   | & \rexpr_1 ~ \rexpr_2         & \text{function application} \\\\
    \\\\
\end{array}
\\]

\\[
\begin{array}{lrll}
    \Arrow{\rtype_1}{\rtype_2} & := & \Pi{x:\rtype_1}{\rtype_2} & \text{non-dependent function types} \\\\
    \lam{x}{\rexpr}            & := & \lam{x:?}{\rexpr}         & \text{functions (without an annotation)} \\\\
\end{array}
\\]

### Terms

The core term syntax skips holes, ensuring that everything is fully elaborated:

\\[
\begin{array}{rrll}
    \texpr,\ttype   & ::= & x                           & \text{variables} \\\\
                    &   | & \Type_i                     & \text{universe of types ($i \in \mathbb{N}$)} \\\\
                    &   | & \const                      & \text{constants} \\\\
                    &   | & \texpr : \ttype             & \text{term annotated with a type} \\\\
                    &   | & \Pi{x:\ttype_1}{\ttype_2}   & \text{dependent function type} \\\\
                    &   | & \lam{x:\ttype}{\texpr}      & \text{functions} \\\\
                    &   | & \texpr_1 ~ \texpr_2         & \text{function application} \\\\
    \\\\
\end{array}
\\]

### Values

In order to make it clear what is 'stuck' and what still needs to be evaluated,
we separate our syntax into weak head normal forms (\\(\wexpr\\)),
and neutral terms (\\(\nexpr\\)):

\\[
\begin{array}{rrll}
    \vexpr,\vtype   & ::= & \wexpr                      & \text{weak head normal forms} \\\\
                    &   | & \nexpr                      & \text{neutral terms} \\\\
    \\\\
    \nexpr,\ntype   & ::= & x                           & \text{variables} \\\\
                    &   | & \nexpr ~ \texpr             & \text{function application} \\\\
    \\\\
    \wexpr,\wtype   & ::= & \Type_i                     & \text{universe of types ($i \in \mathbb{N}$)} \\\\
                    &   | & \const                      & \text{constants} \\\\
                    &   | & \Pi{x:\vtype_1}{\vtype_2}   & \text{dependent function type} \\\\
                    &   | & \lam{x:\vtype}{\vexpr}      & \text{functions} \\\\
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

We take a _bidirectional_ approach to type checking, splitting it into two
phases: type checking and type inference. This makes the flow of information
through the type checker clear and relatively easy to reason about.
Normalization happens after inference, and before types are fed back in to be
used during type checkiong.

With that in mind, the next sections will describe the following judgements:

| name                              | notation                                             | inputs                                   | outputs                    |
|-----------------------------------|------------------------------------------------------|------------------------------------------|----------------------------|
| [normalization](#normalization)   | \\(\eval{ \Gamma }{ \texpr }{ \vexpr }\\)            | \\(\Gamma\\), \\(\rexpr\\)               | \\(\vexpr\\)               |
| [type checking](#type-checking)   | \\(\check{ \Gamma }{ \rexpr }{ \vtype }{ \texpr }\\) | \\(\Gamma\\), \\(\rexpr\\), \\(\vtype\\) | \\(\texpr\\)               |
| [type inference](#type-inference) | \\(\infer{ \Gamma }{ \rexpr }{ \vtype }{ \texpr }\\) | \\(\Gamma\\), \\(\rexpr\\)               | \\(\vtype\\), \\(\texpr\\) |

Normalization stands on its own, but both checking and inference are mutually
dependent on each other. Care has been taken to design the judgments so that
they are _syntax-directed_, meaning that an algorithm can be clearly derived
from them.

Here is a rough overview of how Pikelet terms are checked:

```
                (from parser)
                      |
                      v
     +------------ RawTerm -----------+
     |                                |
     v                                v
Type Inference <- - - - - - -> Type checking
     |                                ^
     |                                |
   Term                             Value
     |                                |
     +-------> Normalization ---------+
     |
     |
     v
 (to compiler)
```

<!-- TODO: use SVG for this diagram -->

### Elaboration

Elaboration is the process of filling in missing information that the
programmer omitted in the original code, generally based on the results
of type inference.

In Pikelet's judgements the elaborated terms are denoted after the
diamond: \\(\rhd\\). At the moment not much is added - only the missing
type annotations on function parameters. In the future this could be extended
filling in type class instances and implicit arguments.

### Normalization

Here we describe how we normalize elaborated terms under the assumptions
in the context.

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
    \rule{E-CONST}{}{
        \eval{ \Gamma }{ \const }{ \const }
    }
    \\\\[2em]
    \rule{E-VAR-ANN}{
        x : \vtype \in \Gamma
    }{
        \eval{ \Gamma }{ x }{ x }
    }
    \\\\[2em]
    \rule{E-VAR-DEF}{
        x : \vtype=\texpr \in \Gamma
        \qquad
        \eval{ \Gamma }{ \texpr }{ \vexpr }
    }{
        \eval{ \Gamma }{ x }{ \vexpr }
    }
    \\\\[2em]
    \rule{E-PI}{
        \eval{ \Gamma }{ \ttype_1 }{ \vtype_1 }
        \qquad
        \eval{ \Gamma, x:\vtype_1 }{ \ttype_2 }{ \vtype_2 }
    }{
        \eval{ \Gamma }{ \Pi{x:\ttype_1}{\ttype_2} }{ \Pi{x:\vtype_1}{\vtype_2} }
    }
    \\\\[2em]
    \rule{E-LAM}{
        \eval{ \Gamma }{ \ttype }{ \vtype }
        \qquad
        \eval{ \Gamma, x:\vtype }{ \texpr }{ \vexpr }
    }{
        \eval{ \Gamma }{ \lam{x:\ttype}{\texpr} }{ \lam{x:\vtype}{\vexpr} }
    }
    \\\\[2em]
    \rule{E-APP}{
        \eval{ \Gamma }{ \texpr_1 }{ \lam{x:\vtype_1}{\vexpr_1} }
        \qquad
        \eval{ \Gamma, x:\vtype_1=\vexpr_2 }{ \vexpr_1 }{ \vexpr_1' }
    }{
        \eval{ \Gamma }{ \texpr_1 ~ \texpr_2 }{ \vexpr_1' }
    }
    \\\\[2em]
\end{array}
\\]

### Type checking

This judgement checks that the given term has the expected type and returns its
elaborated form.

\\[
\boxed{
    \check{ \Gamma }{ \rexpr }{ \vtype }{ \texpr }
}
\\\\[2em]
\begin{array}{cl}
    \rule{C-LAM}{
        \infer{ \Gamma,x:\vtype_1 }{ \rexpr }{ \ttype_2 }{ \texpr }
    }{
        \check{ \Gamma }{ \lam{x:?}{\rexpr} }{ \Pi{x:\vtype_1}{\vtype_2} }{ \lam{x:\vtype_1}{\texpr} }
    }
    \\\\[2em]
    \rule{C-CONV}{
        \infer{ \Gamma }{ \rexpr }{ \vtype_2 }{ \texpr }
        \qquad
        \vtype_1 \equiv_{\alpha} \vtype_2
    }{
        \check{ \Gamma }{ \rexpr }{ \vtype_1 }{ \texpr }
    }
    \\\\[2em]
\end{array}
\\]

In C-CONV we flip the direction of the type checker, comparing the type of the
expected term for [alpha equivalence] with the inferred term. Note that we could
alternatively check for subtyping instead of alpha equivalence. This could be
useful for implementing a cumulative universe hierarchy.

[alpha equivalence]: https://en.wikipedia.org/wiki/Lambda_calculus#Alpha_equivalence

### Type inference

Here we define a jugement that synthesizes a type from the given term and
returns its elaborated form.

\\[
\boxed{
    \infer{ \Gamma }{ \rexpr }{ \vtype }{ \texpr }
}
\\\\[2em]
\begin{array}{cl}
    \rule{I-ANN}{
        \infer{ \Gamma }{ \rtype }{ \Type_i }{ \ttype }
        \qquad
        \eval{ \Gamma }{ \ttype }{ \vtype }
        \qquad
        \check{ \Gamma }{ \rexpr }{ \vtype }{ \texpr }
    }{
        \infer{ \Gamma }{ \rexpr:\rtype }{ \Type_{i+1} }{ \texpr:\ttype }
    }
    \\\\[2em]
    \rule{I-TYPE}{}{
        \infer{ \Gamma }{ \Type_i }{ \Type_{i+1} }{ \Type_i }
    }
    \\\\[2em]
    \rule{I-VAR-ANN}{
        x : \vtype \in \Gamma
    }{
        \infer{ \Gamma }{ x }{ \vtype }{ x }
    }
    \\\\[2em]
    \rule{I-VAR-DEF}{
        x : \vtype=\texpr \in \Gamma
        \qquad
        \infer{ \Gamma }{ \texpr }{ \vexpr }
    }{
        \infer{ \Gamma }{ x }{ \vtype }{ x }
    }
    \\\\[2em]
    \rule{I-PI}{
        \infer{ \Gamma }{ \rtype_1 }{ \Type_i }{ \ttype_1 }
        \qquad
        \eval{ \Gamma }{ \ttype_1 }{ \vtype_1 }
        \qquad
        \check{ \Gamma, x:\vtype_1 }{ \rtype_2 }{ \Type_j }{ \ttype_2 }
    }{
        \infer{ \Gamma }{ \Pi{x:\rtype_1}{\rtype_2} }{ \Type_{\max(i,j)} }{ \Pi{x:\ttype_1}{\ttype_2} }
    }
    \\\\[2em]
    \rule{I-LAM}{
        \infer{ \Gamma }{ \rtype }{ \Type_i }{ \ttype }
        \qquad
        \eval{ \Gamma }{ \ttype }{ \vtype_1 }
        \qquad
        \check{ \Gamma, x:\vtype_1 }{ \rexpr}{ \vtype_2 }{ \texpr }
    }{
        \infer{ \Gamma }{ \lam{x:\rtype}{\rexpr} }{ \Pi{x:\vtype_1}{\vtype_2} }{ \lam{x:\ttype}{\texpr} }
    }
    \\\\[2em]
    \rule{I-APP}{
        \infer{ \Gamma }{ \rexpr_1 }{ \Pi{x:\vtype_1}{\vtype_2} }{ \texpr_1 }
        \qquad
        \check{ \Gamma }{ \rexpr_2 }{ \vtype_1 }{ \texpr_2 }
        \qquad
        \eval{ \Gamma, x:\vtype_1=\texpr_2 }{ \vtype_2 }{ \vtype_2' }
    }{
        \infer{ \Gamma }{ \rexpr_1 ~ \rexpr_2 }{ \vtype_2' }{ \texpr_1 ~ \texpr_2 }
    }
    \\\\[2em]
\end{array}
\\]
