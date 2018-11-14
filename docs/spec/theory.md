---
id: theory
title: Theory
sidebar_label: Theory
---

A formalization of the semantics for type checking and normalizing Pikelet.

## Introduction

At its core, Pikelet is a dependently typed lambda calculus with a cumulative
universe hierarchy with explicit level shifts.

> **Note:**
> This document is intended for those who are interested in looking deeper into the formal foundations of Pikelet.
> You _don't_ need to understand this for general use of Pikelet, so feel free to skip this document if that is easier.
> We will however make an effort to explain some of the notation we use here, and point to resources that might help if this piques your curiosity!

### Notation

We use a combination of some [BNF][bnf]-style syntax definitions with
[natural deduction](natural-deduction) rules to define our language. This
combination of notation is sometimes referred to as _computer science
metanotation_ and is, alas, a little hard to pin down [as conventions vary][guy-steele-presentation]
between papers and authors. The general rules stay the same however, and once
you learn to read them they are much more succinct than an actual implementation
could be, and are an invaluable tool for quickly getting a high-level overview
of a programming language's semantics.

> **TODO:**
> Describe BNF, natural deduction rules, overbars, variable binding, etc.

Some handy links:

- [Crash Course on Notation in Programming Language Theory](http://siek.blogspot.com.au/2012/07/crash-course-on-notation-in-programming.html)
- [A practitioner’s guide to reading programming languages papers](https://blog.acolyer.org/2018/01/26/a-practitioners-guide-to-reading-programming-languages-papers/)
- [A path to enlightenment in Programming Language Theory](http://steshaw.org/plt/)

[bnf]: https://en.wikipedia.org/wiki/Backus%E2%80%93Naur_form
[natural-deduction]: https://en.wikipedia.org/wiki/Natural_deduction
[guy-steele-presentation]: https://www.youtube.com/watch?v=7HKbjYqqPPQ

### Where is the soundness proof?

Here we are only defining the rules of our language's type checking and
evaluation. Further work needs to be done to verify that our system actually
satisfies certain interesting [type soundness properties][type-soundness],
like progress, preservation, [strong normalization][normalization-property],
etc. If you would like to discuss this with us, please check out
[the relevant github issue][formalization-issue]!

[type-soundness]: https://en.wikipedia.org/wiki/Type_safety
[normalization-property]: https://en.wikipedia.org/wiki/Normalization_property_(abstract_rewriting)
[formalization-issue]: https://github.com/pikelet-lang/pikelet/issues/39

## Syntax

### Raw terms

$$
% Haskell-style append https://tex.stackexchange.com/questions/4194/how-to-typeset-haskell-operator-and-friends
\gedf\doubleplus{+\kern-1.3ex+\kern0.8ex}
% Small caps https://github.com/mathjax/MathJax-docs/wiki/Small-caps-%5Ctextsc-in-MathJaxx
\gedf\sc#1{\dosc#1\csod}
\gedf\dosc#1#2\csod{{\rm #1{\small #2}}}
\
\gedf\rule[3]{ \dfrac{ ~~#2~~ }{ ~~#3~~ } & \Tiny{\text{(#1)}} }
\
\DeclareMathOperator{\max}{max}
\DeclareMathOperator{\field}{field}
\DeclareMathOperator{\fieldty}{fieldty}
\DeclareMathOperator{\fieldsubst}{fieldsubst}
\DeclareMathOperator{\Match}{\sc{MATCH}}
\DeclareMathOperator{\shift}{shift}
\
% Judgments
\gedf\eval[3]{ #1 \vdash #2 \hookrightarrow #3 }
\gedf\check[4]{ #1 \vdash #2 \uparrow #3 \leadsto #4 }
\gedf\infer[4]{ #1 \vdash #2 \downarrow #3 \leadsto #4 }
\gedf\subty[3]{ #1 \vdash #2 \preccurlyeq #3 }
\gedf\match[3]{ \Match(#1,#2) \Longrightarrow #3 }
\gedf\checkpat[5]{ #1 \vdash #2 \uparrow #3 \leadsto #4 \Longrightarrow #5 }
\gedf\inferpat[5]{ #1 \vdash #2 \downarrow #3 \leadsto #4 \Longrightarrow #5 }
\
% Metavariables
\gedf\rexpr{r} % raw expressions
\gedf\rtype{R} % raw types
\gedf\rpat{s}  % raw patterns
\
\gedf\texpr{t} % expressions
\gedf\ttype{T} % types
\gedf\tpat{p}  % patterns
\
\gedf\vexpr{v} % value expressions
\gedf\vtype{V} % value types
\gedf\wexpr{w} % whnf expressions
\gedf\wtype{W} % whnf types
\gedf\nexpr{n} % neutral expressions
\gedf\ntype{N} % neutral types
\
\gedf\ctx{\Gamma} % contexts
\
% Keywords
\gedf\kw[1]{ \mathsf{#1} }
\
% Term and Type constructors
\gedf\label{l}
\gedf\binder{x}
\gedf\var[1]{x^\wedge{#1}}
\gedf\Type[1]{\kw{Type}^\wedge{#1}}
\gedf\Arrow[2]{ #1 \rightarrow #2 }
\gedf\Pi[2]{ \Arrow{(#1)}{#2} }
\gedf\lam[2]{ \kw{\lambda} #1 . #2 }
\gedf\app[2]{ #1 ~ #2 }
\gedf\case[2]{ \kw{case} ~ #1 \left\{ #2 \right\} }
\gedf\RecordCons[2]{ \kw{Record} \left\{ #1; #2 \right\} }
\gedf\RecordEmpty{ \kw{Record} \left\{\right\} }
\gedf\as{ ~ \kw{as} ~ }
\gedf\record[1]{ \kw{record} \left\{ #1 \right\} }
\gedf\proj[3]{ #1.#2^\wedge{#3} }
\gedf\subst[3]{ #1 ~ [#2 \rightarrow #3] }
\
% Items
\gedf\declItem[2]{ #1 : #2 }
\gedf\defnItem[2]{ #1 = #2 }
\
% Contexts
\gedf\emptyCtx{ \varnothing }
\gedf\composeCtx[2]{ #1 \sim #2 }
\gedf\extendCtx[2]{ #1, #2 }
\
\begin{array}{rrll}
    \rexpr,\rtype   & ::= & \var{i}                             & \text{variables ($i \in \mathbb{N}$)} \\
                    &   | & \Type{i}                            & \text{universe of types ($i \in \mathbb{N}$)} \\
                    &   | & ?                                   & \text{holes} \\
                    &   | & \rexpr : \rtype                     & \text{term annotated with a type} \\
                    &   | & \Pi{\binder:\rtype_1}{\rtype_2}     & \text{dependent function type} \\
                    &   | & \lam{\binder:\rtype}{\rexpr}        & \text{functions} \\
                    &   | & \app{\rexpr_1}{\rexpr_2}            & \text{function application} \\
                    &   | & \case{\rexpr}{\overline{\rpat_i \rightarrow \rexpr_i}^{;}} & \text{case expressions} \\
                    &   | & \RecordCons{\label \as \binder:\rtype_1}{\rtype_2} & \text{record type extension} \\
                    &   | & \RecordEmpty                        & \text{empty record type} \\
                    &   | & \record{\label=\rexpr_1, \rexpr_2}  & \text{record extension} \\
                    &   | & \record{}                           & \text{empty record} \\
                    &   | & \proj{\rexpr}{\label}{i}            & \text{record projection ($i \in \mathbb{N}$)} \\
    \\
    \rpat           & ::= & \binder                             & \text{binder pattern} \\
                    &   | & \rpat : \rtype                      & \text{pattern annotated with a type} \\
                %   &   | & \record{\label=\rpat_1, \rpat_2}    & \text{record extension pattern} \\
                %   &   | & \record{}                           & \text{empty record pattern} \\
    \\
\end{array}
$$

$$
\begin{array}{lrll}
    \Arrow{\rtype_1}{\rtype_2} & := & \Pi{\binder:\rtype_1}{\rtype_2} & \text{non-dependent function types} \\
    \lam{\binder}{\rexpr}      & := & \lam{\binder:?}{\rexpr}         & \text{functions (without an annotation)} \\
\end{array}
$$

### Terms

The core term syntax skips holes, ensuring that everything is fully elaborated:

$$
\begin{array}{rrll}
    \texpr,\ttype   & ::= & \var{i}                             & \text{variables ($i \in \mathbb{N}$)} \\
                    &   | & \Type{i}                            & \text{universe of types ($i \in \mathbb{N}$)} \\
                    &   | & \texpr : \ttype                     & \text{term annotated with a type} \\
                    &   | & \Pi{\binder:\ttype_1}{\ttype_2}     & \text{dependent function type} \\
                    &   | & \lam{\binder:\ttype}{\texpr}        & \text{functions} \\
                    &   | & \app{\texpr_1}{\texpr_2}            & \text{function application} \\
                    &   | & \case{\texpr}{\overline{\tpat_i \rightarrow \texpr_i}^{;}} & \text{case expressions} \\
                    &   | & \RecordCons{\label \as \binder:\ttype_1}{\ttype_2} & \text{record type extension} \\
                    &   | & \RecordEmpty                        & \text{empty record type} \\
                    &   | & \record{\label=\texpr_1, \texpr_2}  & \text{record extension} \\
                    &   | & \record{}                           & \text{empty record} \\
                    &   | & \proj{\texpr}{\label}{i}            & \text{record projection ($i \in \mathbb{N}$)} \\
    \\
    \tpat           & ::= & \binder                             & \text{binder pattern} \\
                    &   | & \tpat : \ttype                      & \text{pattern annotated with a type} \\
                    &   | & \record{\label=\tpat_1, \tpat_2}    & \text{record extension pattern} \\
                    &   | & \record{}                           & \text{empty record pattern} \\
    \\
\end{array}
$$

### Values

In order to make it clear what is 'stuck' and what still needs to be evaluated,
we separate our syntax into [weak head normal forms][whnf-wikipedia] ($\wexpr$),
and neutral terms ($\nexpr$):

$$
\begin{array}{rrll}
    \vexpr,\vtype   & ::= & \wexpr                              & \text{weak head normal forms} \\
                    &   | & \nexpr                              & \text{neutral terms} \\
    \\
    \nexpr,\ntype   & ::= & \var{i}                             & \text{variables ($i \in \mathbb{N}$)} \\
                    &   | & \app{\nexpr}{\texpr}                & \text{function application} \\
                    &   | & \case{\nexpr}{\overline{\tpat_i \rightarrow \texpr_i}^{;}} & \text{case expressions} \\
                    &   | & \proj{\nexpr}{\label}{i}            & \text{record projection ($i \in \mathbb{N}$)} \\
    \\
    \wexpr,\wtype   & ::= & \Type{i}                            & \text{universe of types ($i \in \mathbb{N}$)} \\
                    &   | & \Pi{\binder:\vtype_1}{\vtype_2}     & \text{dependent function type} \\
                    &   | & \lam{\binder:\vtype}{\vexpr}        & \text{functions} \\
                    &   | & \RecordCons{\label \as \binder:\vtype_1}{\vtype_2} & \text{record type extension} \\
                    &   | & \RecordEmpty                        & \text{empty record type} \\
                    &   | & \record{\label=\vexpr_1, \vexpr_2}  & \text{record extension} \\
                    &   | & \record{}                           & \text{empty record} \\
    \\
\end{array}
$$

[whnf-wikipedia]: https://en.wikipedia.org/wiki/Lambda_calculus_definition#Weak_head_normal_form

### Contexts

As we type check terms, we'll be passing over bindings like lambdas and pi types.
Contexts allow us to keep track of the bound parameters,
even though we don't know the exact values these will eventually take during normalization.

$$
\begin{array}{rrll}
    \ctx    & ::= & \emptyCtx                                      & \text{the empty context} \\
            &   | & \extendCtx{\ctx}{\declItem{\binder}{\vtype}}   & \text{context extended with a declaration} \\
            &   | & \extendCtx{\ctx}{\defnItem{\binder}{\texpr}}   & \text{context extended with a definition} \\
\end{array}
$$

## Semantics

We take a _bidirectional_ approach to type checking, splitting it into two
phases: type checking and type inference. This makes the flow of information
through the type checker clear and relatively easy to reason about.
Normalization happens after inference, and before types are fed back in to be
used during type checking.

With that in mind, the next sections will describe the following judgments:

| Name                                                      | Notation                                                 | Inputs                         | Outputs                     |
|-----------------------------------------------------------|----------------------------------------------------------|--------------------------------|-----------------------------|
| [normalization](#normalization)                           | $\eval{ \ctx }{ \texpr }{ \vexpr }$                      | $\ctx$, $\rexpr$               | $\vexpr$                    |
| [type checking](#type-checking)                           | $\check{ \ctx }{ \rexpr }{ \vtype }{ \texpr }$           | $\ctx$, $\rexpr$, $\vtype$     | $\texpr$                    |
| [type inference](#type-inference)                         | $\infer{ \ctx }{ \rexpr }{ \vtype }{ \texpr }$           | $\ctx$, $\rexpr$               | $\vtype$, $\texpr$          |
| [subtyping](#subtyping)                                   | $\subty{ \ctx }{ \vtype_1 }{ \vtype_2 }$                 | $\ctx$, $\vtype_1$, $\vtype_2$ |                             |
| [pattern matching](#pattern-matching)                     | $\match{ \wexpr }{ \tpat }{ \theta }$                    | $\wexpr$, $\tpat$              | $\theta$                    |
| [type checking of patterns](#type-checking-of-patterns)   | $\checkpat{ \ctx }{ \rpat }{ \vtype }{ \tpat }{ \ctx' }$ | $\ctx$, $\rpat$, $\vtype$      | $\tpat$, $\ctx'$            |
| [type inference of patterns](#type-inference-of-patterns) | $\inferpat{ \ctx }{ \rpat }{ \vtype }{ \tpat }{ \ctx' }$ | $\ctx$, $\rpat$,               | $\vtype$, $\tpat$, $\ctx'$  |

Normalization stands on its own, but both checking and inference are mutually
dependent on each other. Care has been taken to design the judgments so that
they are _syntax-directed_, meaning that an algorithm can be clearly derived
from them.

### Elaboration

Elaboration is the process of filling in missing information that the
programmer omitted in the original code, generally based on the results
of type inference.

In Pikelet's judgements the elaborated terms are denoted after the
diamond: $\rhd$. At the moment not much is added - only the missing
type annotations on function parameters. In the future this could be extended
filling in type class instances and implicit arguments.

### Normalization

Here we describe how we normalize elaborated terms under the assumptions
in the context.

$$
\boxed{
    \eval{ \ctx }{ \texpr }{ \vexpr }
}
\\[2em]
\begin{array}{cl}
    \rule{E-ANN}{
        \eval{ \ctx }{ \texpr }{ \vexpr }
    }{
        \eval{ \ctx }{ \texpr:\ttype }{ \vexpr }
    }
    \\[2em]
    \rule{E-TYPE}{}{
        \eval{ \ctx }{ \Type{i} }{ \Type{i} }
    }
    \\[2em]
    \rule{E-VAR}{
        \defnItem{\binder}{\texpr} \notin \ctx
    }{
        \eval{ \ctx }{ \var{i} }{ \var{i} }
    }
    \\[2em]
    \rule{E-VAR-DEF}{
        \defnItem{\binder}{\texpr} \in \ctx
        \qquad
        \eval{ \ctx }{ \texpr }{ \vexpr }
    }{
        \eval{ \ctx }{ \var{i} }{ \shift(\vexpr,i) }
    }
    \\[2em]
    \rule{E-PI}{
        \eval{ \ctx }{ \ttype_1 }{ \vtype_1 }
        \qquad
        \eval{ \ctx }{ \ttype_2 }{ \vtype_2 }
    }{
        \eval{ \ctx }{ \Pi{\binder:\ttype_1}{\ttype_2} }{ \Pi{\binder:\vtype_1}{\vtype_2} }
    }
    \\[2em]
    \rule{E-LAM}{
        \eval{ \ctx }{ \ttype }{ \vtype }
        \qquad
        \eval{ \ctx }{ \texpr }{ \vexpr }
    }{
        \eval{ \ctx }{ \lam{\binder:\ttype}{\texpr} }{ \lam{\binder:\vtype}{\vexpr} }
    }
    \\[2em]
    \rule{E-APP}{
        \eval{ \ctx }{ \texpr_1 }{ \lam{\binder:\vtype_1}{\vexpr_1} }
        \qquad
        \eval{ \ctx }{ \subst{\vexpr_1}{\binder}{\texpr_2} }{ \vexpr_3 }
    }{
        \eval{ \ctx }{ \app{\texpr_1}{\texpr_2} }{ \vexpr_3 }
    }
    \\[2em]
    \rule{E-CASE}{
        \eval{ \ctx }{ \nexpr }{ \nexpr' }
    }{
        \eval{ \ctx }{ \case{\nexpr}{\overline{\tpat_i \rightarrow \texpr_i}^{;}} }
            { \case{\nexpr'}{\overline{\tpat_i \rightarrow \texpr_i}^{;}} }
    }
    \\[2em]
    \rule{E-CASE-MATCH}{
        \eval{ \ctx }{ \nexpr }{ \wexpr }
        \qquad
        \match{ \wexpr }{ \tpat_i }{ \theta }
        \qquad
        \eval{ \ctx }{ \texpr_i ~ \theta }{ \vexpr_i }
    }{
        \eval{ \ctx }{ \case{\nexpr}{\overline{\tpat_i \rightarrow \texpr_i}^{;}} }{ \vexpr_i }
    }
    \\[2em]
    \rule{E-RECORD-TYPE}{
        \eval{ \ctx }{ \ttype_1 }{ \vtype_1 }
        \qquad
        \eval{ \ctx }{ \ttype_2 }{ \vtype_2 }
    }{
        \eval{ \ctx }{ \RecordCons{\label \as \binder:\ttype_1}{\ttype_2} }{ \RecordCons{\label \as \binder:\vtype_1}{\vtype_2} }
    }
    \\[2em]
    \rule{E-RECORD}{
        \eval{ \ctx }{ \texpr_1 }{ \vexpr_1 }
        \qquad
        \eval{ \ctx }{ \texpr_2 }{ \vexpr_2 }
    }{
        \eval{ \ctx }{ \record{\label=\texpr_1, \texpr_2} }{ \record{\label=\vexpr_1, \vexpr_2} }
    }
    \\[2em]
    \rule{E-EMPTY-RECORD-TYPE}{}{
        \eval{ \ctx }{ \RecordEmpty }{ \RecordEmpty }
    }
    \\[2em]
    \rule{E-EMPTY-RECORD}{}{
        \eval{ \ctx }{ \record{} }{ \record{} }
    }
    \\[2em]
    \rule{E-PROJ}{
        \eval{ \ctx }{ \texpr_1 }{ \vexpr_1 }
        \qquad
        \vexpr_2 = \field(\label, \vexpr_1)
    }{
        \eval{ \ctx }{ \proj{\texpr_1}{\label}{i} }{ \vexpr_2 }
    }
    \\[2em]
\end{array}
$$

We define $\field(-,-)$ like so:

$$
\begin{array}{lrll}
    \field(\label_1, \record{\label_2 = \vexpr_1, \vexpr_2}) & = & \vexpr_1 & \text{if} ~ \label_1 \equiv \label_2 \\
    \field(\label_1, \record{\label_2 = \vexpr_1, \vexpr_2}) & = & \field(\label_1, \vexpr_2) \\
\end{array}
$$

### Type checking

This judgement checks that the given term has the expected type and returns its
elaborated form.

$$
\boxed{
    \check{ \ctx }{ \rexpr }{ \vtype }{ \texpr }
}
\\[2em]
\begin{array}{cl}
    \rule{C-LAM}{
        \infer{ \extendCtx{\ctx}{\declItem{\binder}{\vtype_1}} }{ \rexpr }{ \ttype_2 }{ \texpr }
    }{
        \check{ \ctx }{ \lam{\binder}{\rexpr} }{ \Pi{\binder:\vtype_1}{\vtype_2} }{ \lam{\binder:\vtype_1}{\texpr} }
    }
    \\[2em]
    \rule{C-CASE}{
        \infer{ \ctx }{ \rexpr }{ \vtype_1 }{ \texpr }
        \qquad
        \overline{
            % TODO: impl pattern checks
            ~
            \check{ \ctx }{ \rpat_i }{ \vtype_1 }{ \tpat_i } \Rightarrow \ctx'
            \qquad
            \check{ \composeCtx{\ctx}{\ctx'} }{ \rexpr_i }{ \vtype_2 }{ \texpr_i }
            ~
        }
    }{
        \check{ \ctx }{ \case{\rexpr}{\overline{\rpat_i \rightarrow \rexpr_i}^{;}} }{ \vtype_2 }
            { \case{\texpr}{\overline{\tpat_i \rightarrow \texpr_i}^{;}} }
    }
    \\[2em]
    \rule{C-RECORD}{
        \label_1 \equiv \label_2
        \qquad
        \check{ \ctx }{ \rexpr_1 }{ \vtype_1 }{ \texpr_1 }
        \qquad
        \eval{ \ctx }{ \subst{\vtype_2}{\binder}{\texpr_1} }{ \vtype_3 }
        \qquad
        \check{ \ctx }{ \rexpr_2 }{ \vtype_3 }{ \texpr_2 }
    }{
        \check{ \ctx }{ \record{\label_1=\rexpr_1, \rexpr_2} }
            { \RecordCons{\label_2 \as \binder:\vtype_1}{\vtype_2} }
            { \record{\label_1=\texpr_1, \texpr_2} }
    }
    \\[2em]
    \rule{C-CONV}{
        \infer{ \ctx }{ \rexpr }{ \vtype_2 }{ \texpr }
        \qquad
        \subty{ \ctx }{ \vtype_1 }{ \vtype_2 }
    }{
        \check{ \ctx }{ \rexpr }{ \vtype_1 }{ \texpr }
    }
    \\[2em]
\end{array}
$$

### Type inference

Here we define a judgement that synthesizes a type from the given term and
returns its elaborated form.

$$
\boxed{
    \infer{ \ctx }{ \rexpr }{ \vtype }{ \texpr }
}
\\[2em]
\begin{array}{cl}
    \rule{I-ANN}{
        \infer{ \ctx }{ \rtype }{ \Type{i} }{ \ttype }
        \qquad
        \eval{ \ctx }{ \ttype }{ \vtype }
        \qquad
        \check{ \ctx }{ \rexpr }{ \vtype }{ \texpr }
    }{
        \infer{ \ctx }{ \rexpr:\rtype }{ \Type{i+1} }{ \texpr:\ttype }
    }
    \\[2em]
    \rule{I-TYPE}{}{
        \infer{ \ctx }{ \Type{i} }{ \Type{(i+1)} }{ \Type{i} }
    }
    \\[2em]
    \rule{I-VAR}{
        \declItem{\binder}{\vtype} \in \ctx
    }{
        \infer{ \ctx }{ \var{i} }{ \shift(\vtype,i) }{ \var{i} }
    }
    \\[2em]
    \rule{I-PI}{
        \infer{ \ctx }{ \rtype_1 }{ \Type{i} }{ \ttype_1 }
        \qquad
        \eval{ \ctx }{ \ttype_1 }{ \vtype_1 }
        \qquad
        \check{ \extendCtx{\ctx}{\declItem{\binder}{\vtype_1}} }{ \rtype_2 }{ \Type{j} }{ \ttype_2 }
    }{
        \infer{ \ctx }{ \Pi{\binder:\rtype_1}{\rtype_2} }{ \Type{\max(i,j)} }
            { \Pi{\binder:\ttype_1}{\ttype_2} }
    }
    \\[2em]
    \rule{I-LAM}{
        \infer{ \ctx }{ \rtype }{ \Type{i} }{ \ttype }
        \qquad
        \eval{ \ctx }{ \ttype }{ \vtype_1 }
        \qquad
        \check{ \extendCtx{\ctx}{\declItem{\binder}{\vtype_1}} }{ \rexpr}{ \vtype_2 }{ \texpr }
    }{
        \infer{ \ctx }{ \lam{\binder:\rtype}{\rexpr} }
            { \Pi{\binder:\vtype_1}{\vtype_2} }{ \lam{\binder:\ttype}{\texpr} }
    }
    \\[2em]
    \rule{I-APP}{
        \infer{ \ctx }{ \rexpr_1 }{ \Pi{\binder:\vtype_1}{\vtype_2} }{ \texpr_1 }
        \qquad
        \check{ \ctx }{ \rexpr_2 }{ \vtype_1 }{ \texpr_2 }
        \qquad
        \eval{ \ctx }{ \subst{\vtype_2}{\binder}{\texpr_2} }{ \vtype_3 }
    }{
        \infer{ \ctx }{ \app{\rexpr_1}{\rexpr_2} }{ \vtype_3 }{ \app{\texpr_1}{\texpr_2} }
    }
    \\[2em]
    \rule{I-RECORD-TYPE}{
        \infer{ \ctx }{ \rtype_1 }{ \Type{i} }{ \ttype_1 }
        \qquad
        \eval{ \ctx }{ \ttype_1 }{ \vtype_1 }
        \qquad
        \infer{ \extendCtx{\ctx}{\declItem{\binder}{\vtype_1}} }{ \rtype_2 }{ \Type{j} }{ \ttype_2 }
    }{
        \infer{ \ctx }
            { \RecordCons{\label \as \binder:\rtype_1}{\rtype_2} }
            { \Type{\max(i,j)} }
            { \RecordCons{\label \as \binder:\ttype_1}{\ttype_2} }
    }
    \\[2em]
    \rule{I-EMPTY-RECORD-TYPE}{}{
        \infer{ \ctx }{ \RecordEmpty }{ \Type{0} }{ \RecordEmpty }
    }
    \\[2em]
    \rule{I-RECORD}{
        \infer{ \ctx }{ \rexpr_1 }{ \vtype_1 }{ \texpr_1 }
        \qquad
        \infer{ \ctx }{ \rexpr_2 }{ \vtype_2 }{ \texpr_2 }
        \qquad
        \eval{ \ctx }{ \subst{\vtype_2}{\binder}{\texpr_1} }{ \vtype_3 }
    }{
        \infer{ \ctx }{ \record{\label=\rexpr_1, \rexpr_2} }
            { \RecordCons{\label \as \binder:\vtype_1}{\vtype_3} }
            { \record{\label=\texpr_1, \texpr_2} }
    }
    \\[2em]
    \rule{I-EMPTY-RECORD}{}{
        \infer{ \ctx }{ \record{} }{ \RecordEmpty }{ \record{} }
    }
    \\[2em]
    \rule{I-PROJ}{
        \infer{ \ctx }{ \rexpr }{ \vtype_1 }{ \texpr }
        \qquad
        \vtype_2 = \fieldty(\label, \vtype_1)
        \qquad
        \theta = \fieldsubst(\texpr, \label, i, \vtype_1)
    }{
        \infer{ \ctx }{ \proj{\rexpr}{\label}{i} }{ \vtype_2 ~ \theta }{ \proj{\texpr}{\label}{i} }
    }
    \\[2em]
\end{array}
$$

We define $\fieldty(-,-)$ like so:

$$
\begin{array}{lrll}
    \fieldty(\label_1, \RecordCons{\label_2 : \vtype_1}{\vtype_2}) & = & \vtype_1 & \text{if} ~ \label_1 \equiv \label_2 \\
    \fieldty(\label_1, \RecordCons{\label_2 : \vtype_1}{\vtype_2}) & = & \fieldty(\label_1, \vtype_2) \\
    \\[2em]
\end{array}
$$

In order to ensure that we maintain maintain the proper paths to variables when
we project on them, we define $\fieldsubst(-,-,-,-)$ as:

$$
\begin{array}{lrll}
    \fieldsubst(\texpr, \label_1, i, \RecordCons{\label_2 : \vtype_1}{\vtype_2}) & =
        & [] & \text{if} ~ \label_1 \equiv \label_2 \\
    \fieldsubst(\texpr, \label_1, i, \RecordCons{\label_2 : \vtype_1}{\vtype_2}) & =
        & \fieldsubst(\texpr, \label_1, \vtype_2) \doubleplus [ \label_2 \rightarrow \proj{\texpr}{\label_2}{i} ] \\
    \\[2em]
\end{array}
$$

### Subtyping

$$
\boxed{
    \subty{ \ctx }{ \vtype_1 }{ \vtype_2 }
}
\\[2em]
\begin{array}{cl}
    \rule{ST-TYPE}{
        i \leqslant j
    }{
        \subty{ \ctx }{ \Type{i} }{ \Type{j} }
    }
    \\[2em]
    \rule{ST-PI}{
        \subty{ \ctx }{ \vtype_2 }{ \vtype_1 }
        \qquad
        \subty{ \extendCtx{\ctx}{\declItem{\binder}{\vtype_2}} }{ \vtype_3 }{ \vtype_4 }
    }{
        \subty{ \ctx }{ \Pi{\binder:\vtype_1}{\vtype_2} }
            { \Pi{\binder:\vtype_3}{\vtype_4} }
    }
    \\[2em]
    \rule{ST-RECORD-TYPE}{
        \subty{ \ctx }{ \vtype_1 }{ \vtype_3 }
        \qquad
        \subty{ \extendCtx{\ctx}{\declItem{\binder}{\vtype_1}} }{ \vtype_2 }{ \vtype_4 }
    }{
        \subty{ \ctx }{ \RecordCons{\label \as \binder:\vtype_1}{\vtype_2} }
            { \RecordCons{\label \as \binder:\vtype_3}{\vtype_4} }
    }
    \\[2em]
    \rule{ST-EMPTY-RECORD-TYPE}{}{
        \subty{ \ctx }{ \RecordEmpty }{ \RecordEmpty }
    }
    \\[2em]
    \rule{ST-ALPHA-EQ}{
        \vtype_1 \equiv_{\alpha} \vtype_2
    }{
        \subty{ \ctx }{ \vtype_1 }{ \vtype_2 }
    }
    \\[2em]
\end{array}
$$

### Universe shifting

We implement explicit level shifts, giving us something like what Conor McBride
describes in his blog post, [universe hierarchies][universe-hierarchies].

We define $\shift(-,-)$ for values:

$$
\begin{array}{llrl}
    \shift(\var{i},                                             & j) & = & \var{i} \\
    \shift(\app{\nexpr}{\texpr},                                & j) & = & \app{\shift(\nexpr, j)}{\shift(\texpr, j)} \\
    \shift(\case{\nexpr}{\overline{\tpat_i \rightarrow \texpr_i}^{;}}, & j) & = &
        % FIXME: define pattern shifting
        \case{\shift(\nexpr, j)}{\overline{\shift(\tpat_i, j) \rightarrow \shift(\texpr_i, j)}^{;}} \\
    \shift(\proj{\nexpr}{\label}{i},                            & j) & = & \proj{\shift(\nexpr, j)}{\label}{i} \\
    \shift(\Type{i},                                            & j) & = & \Type{(i + j)} \\
    \shift(\Pi{\binder:\vtype_1}{\vtype_2},                     & j) & = & \Pi{\binder:\shift(\vtype_1, j)}{\shift(\vtype_2, j)} \\
    \shift(\lam{\binder:\vtype}{\vexpr},                        & j) & = & \lam{\binder:\shift(\vtype, j)}{\shift(\vexpr, j)} \\
    \shift(\RecordCons{\label \as \binder:\vtype_1}{\vtype_2},  & j) & = & \RecordCons{\label \as \binder:\shift(\vtype_1, j)}{\shift(\vtype_2, j)} \\
    \shift(\RecordEmpty,                                        & j) & = & \RecordEmpty \\
    \shift(\record{\label=\vexpr_1, \vexpr_2},                  & j) & = & \record{\label=\shift(\vexpr_1, j), \shift(\vexpr_2, j)} \\
    \shift(\record{},                                           & j) & = & \record{} \\
    \\[2em]
\end{array}
$$

> **NOTE**:
> We might want to investigate making this shifting operator more expressive and
> 'first class', perhaps as was described in [Dependently typed lambda calculus
> with a lifting operator][dtlc-with-lifts]. For now this seems to be expressive
> enough for most use cases that our users might run into.

[universe-hierarchies]: https://pigworker.wordpress.com/2015/01/09/universe-hierarchies/
[dtlc-with-lifts]: http://www-sop.inria.fr/members/Damien.Rouhling/data/internships/M1Report.pdf

### Pattern matching

This judement takes an expression $\wexpr$ in weak head normal form, and a
pattern $\tpat$ and returns a substitution $\theta$ with the matched bindings.

$$
\boxed{
    \match{ \wexpr }{ \tpat }{ \theta }
}
\\[2em]
\begin{array}{cl}
    \rule{M-VAR}{}{
        \match{ \wexpr }{ \binder }{ [\binder \rightarrow \wexpr] }
    }
    \\[2em]
% TODO:
%   \rule{M-RECORD}{
%       \match{ \wexpr_1 }{ \tpat_1 }{ \theta_1 }
%       \qquad
%       \match{ \wexpr_2 }{ \tpat_2 }{ \theta_2 }
%   }{
%       \match{ \record{\label=\wexpr_1, \wexpr_2} }{ \record{\label=\tpat_1, \tpat_2} }{ \theta_1 \doubleplus \theta_2 }
%   }
%   \\[2em]
%   \rule{M-EMPTY-RECORD}{}{
%       \match{ \record{} }{ \record{} }{ [] }
%   }
%   \\[2em]
\end{array}
$$

### Type checking of patterns

$$
\boxed{
    \checkpat{ \ctx }{ \rpat }{ \vtype }{ \tpat }{ \ctx' }
}
\\[2em]
\begin{array}{cl}
    \rule{CP-BINDER}{}{
        \checkpat{ \ctx }{ \binder }{ \vtype }{ \binder }{ \binder : \vtype }
    }
    \\[2em]
    \rule{CP-CONV}{
        \inferpat{ \ctx }{ \rpat }{ \vtype_2 }{ \tpat }{ \ctx' }
        \qquad
        \subty{ \ctx }{ \vtype_1 }{ \vtype_2 }
    }{
        \checkpat{ \ctx }{ \rpat }{ \vtype_1 }{ \tpat }{ \ctx' }
    }
    \\[2em]
\end{array}
$$

### Type inference of patterns

$$
\boxed{
    \inferpat{ \ctx }{ \rpat }{ \vtype }{ \tpat }{ \ctx' }
}
\\[2em]
\begin{array}{cl}
    \rule{IP-ANN}{
        \infer{ \ctx }{ \rtype }{ \Type{i} }{ \ttype }
        \qquad
        \eval{ \ctx }{ \ttype }{ \vtype }
        \qquad
        \checkpat{ \ctx }{ \rpat }{ \vtype }{ \rpat }{ \ctx' }
    }{
        \inferpat{ \ctx }{ \rpat : \rtype }{ \rtype }{ \rpat : \rtype }{ \ctx' }
    }
    \\[2em]
\end{array}
$$

> **TODO:**
>
> - Pattern matching coverage checking
> - Ensure that parametericity is maintained. Should we forbid [pattern matching
>   directly on types][type-patterns]? McBride seems to [think we can have our
>   cake and eat it][type-patterns-mcbride]!

[type-patterns]: https://stackoverflow.com/questions/45439486/pattern-matching-on-type-in-idris
[type-patterns-mcbride]: https://stackoverflow.com/questions/23220884/why-is-typecase-a-bad-thing/26012264#26012264
