# Theory

A formalization of the semantics for type checking and normalizing Pikelet.

## Contents

- [Introduction](#introduction)
  - [Notation](#notation)
  - [Where is the soundness proof?](#where-is-the-soundness-proof)
- [Syntax](#syntax)
  - [Raw terms](#raw-terms)
  - [Elaborated terms](#elaborated-terms)
  - [Values](#values)
  - [Contexts](#contexts)
- [Semantics](#semantics)
  - [Elaboration](#elaboration)
  - [Normalization](#normalization)
  - [Type checking](#type-checking)
  - [Type inference](#type-inference)
- [Related work](#related-work)
- [Future work](#future-work)

## Introduction

At its core, Pikelet is a dependently typed lambda calculus that has the
following key features:

- bidirectioinal type checking
- dependent functions
- stratified universe hierarchy
- explicit substitutions
- non-dependent records with projections

This feature list will no doubt grow and change as the language evolves and
becomes more expressive. The goal of this document is to provide a birds-eye
view of the language in its current form, and help guide future formalization
work.

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

> TODO: Describe BNF, natural deduction rules, overbars, variable binding, etc.

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
[formalization-issue]: https://github.com/brendanzab/pikelet/issues/39

## Syntax

### Raw terms

\\[
\\newcommand{\rule}[3]{ \dfrac{ ~~#2~~ }{ ~~#3~~ } & \Tiny{\text{(#1)}} }
\\
\\DeclareMathOperator{\max}{max}
\\DeclareMathOperator{\field}{field}
\\DeclareMathOperator{\fieldty}{fieldty}
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
% Keywords
\\newcommand{\kw}[1]{ \mathsf{#1} }
\\
% Term and Type constructors
\\newcommand{\Type}{\kw{Type}}
\\newcommand{\Bool}{\kw{Bool}}
\\newcommand{\true}{\kw{true}}
\\newcommand{\false}{\kw{false}}
\\newcommand{\Arrow}[2]{ #1 \rightarrow #2 }
\\newcommand{\Pi}[2]{ (#1) \rightarrow #2 }
\\newcommand{\lam}[2]{ \lambda #1 \rightarrow #2 }
\\newcommand{\app}[2]{ #1 ~ #2 }
\\newcommand{\subst}[3]{ #1 ~ [#2 \rightarrow #3] }
\\newcommand{\ifte}[3]{ \kw{if} ~ #1 ~ \kw{then} ~ #2 ~ \kw{else} ~ #3 }
\\newcommand{\Record}[1]{ (#1) }
\\newcommand{\record}[1]{ \langle #1 \rangle }
\\
% Judgements
\\newcommand{\eval}[3]{ #1 \vdash #2 \Rightarrow #3 }
\\newcommand{\pushSubsts}[5]{ #1 \vdash \subst{#2}{#3}{#4} \mapsto #5 }
\\newcommand{\check}[4]{ #1 \vdash #2 \uparrow #3 \rhd #4 }
\\newcommand{\infer}[4]{ #1 \vdash #2 \downarrow #3 \rhd #4 }
\\
\begin{array}{rrll}
    \rexpr,\rtype   & ::= & x                                   & \text{variables} \\\\
                    &   | & \Type_i                             & \text{universe of types ($i \in \mathbb{N}$)} \\\\
                    &   | & ?                                   & \text{holes} \\\\
                    &   | & \Bool                               & \text{type of booleans} \\\\
                    &   | & \true ~|~ \false                    & \text{boolean values} \\\\
                    &   | & \rexpr : \rtype                     & \text{term annotated with a type} \\\\
                    &   | & \Pi{x:\rtype_1}{\rtype_2}           & \text{dependent function type} \\\\
                    &   | & \lam{x:\rtype}{\rexpr}              & \text{functions} \\\\
                    &   | & \app{\rexpr_1}{\rexpr_2}            & \text{function application} \\\\
                    &   | & \ifte{\rexpr_1}{\rexpr_2}{\rexpr_3} & \text{if expressions} \\\\
                    &   | & \Record{l:\rtype_1, \rtype_2}       & \text{record type extension} \\\\
                    &   | & \Record{}                           & \text{empty record type} \\\\
                    &   | & \record{l=\rexpr_1, \rexpr_2}       & \text{record extension} \\\\
                    &   | & \record{}                           & \text{empty record} \\\\
                    &   | & \rexpr.l                            & \text{record projection} \\\\
    \\\\
\end{array}
\\]

We also define some syntactic sugar for dependent functions and lambdas:

\\[
\begin{array}{lrll}
    \Arrow{\rtype_1}{\rtype_2} & := & \Pi{x:\rtype_1}{\rtype_2} & \text{non-dependent function types} \\\\
    \lam{x}{\rexpr}            & := & \lam{x:?}{\rexpr}         & \text{functions (without an annotation)} \\\\
\end{array}
\\]

### Elaborated terms

The elaborated term syntax skips holes and adds explicit substitutions:

\\[
\begin{array}{rrll}
    \texpr,\ttype   & ::= & x                                   & \text{variables} \\\\
                    &   | & \Type_i                             & \text{universe of types ($i \in \mathbb{N}$)} \\\\
                    &   | & \Bool                               & \text{type of booleans} \\\\
                    &   | & \true ~|~ \false                    & \text{boolean values} \\\\
                    &   | & \texpr : \ttype                     & \text{term annotated with a type} \\\\
                    &   | & \Pi{x:\ttype_1}{\ttype_2}           & \text{dependent function type} \\\\
                    &   | & \lam{x:\ttype}{\texpr}              & \text{functions} \\\\
                    &   | & \app{\texpr_1}{\texpr_2}            & \text{function application} \\\\
                    &   | & \subst{\texpr_1}{x}{\texpr_2}       & \text{explicit substitution} \\\\
                    &   | & \ifte{\texpr_1}{\texpr_2}{\texpr_3} & \text{if expressions} \\\\
                    &   | & \Record{l:\ttype_1, \ttype_2}       & \text{record type extension} \\\\
                    &   | & \Record{}                           & \text{empty record type} \\\\
                    &   | & \record{l=\texpr_1, \texpr_2}       & \text{record extension} \\\\
                    &   | & \record{}                           & \text{empty record} \\\\
                    &   | & \texpr.l                            & \text{record projection} \\\\
    \\\\
\end{array}
\\]

### Values

In order to make it clear what is 'stuck' and what still needs to be evaluated,
we separate our syntax into [weak head normal forms][whnf-wikipedia] (\\(\wexpr\\)),
and neutral terms (\\(\nexpr\\)):

\\[
\begin{array}{rrll}
    \vexpr,\vtype   & ::= & \wexpr                              & \text{weak head normal forms} \\\\
                    &   | & \nexpr                              & \text{neutral terms} \\\\
    \\\\
    \nexpr,\ntype   & ::= & x                                   & \text{variables} \\\\
                    &   | & \app{\nexpr}{\texpr}                & \text{function application} \\\\
                    &   | & \ifte{\nexpr_1}{\texpr_2}{\texpr_3} & \text{if expressions} \\\\
                    &   | & \nexpr.l                            & \text{record projection} \\\\
    \\\\
    \wexpr,\wtype   & ::= & \Type_i                             & \text{universe of types ($i \in \mathbb{N}$)} \\\\
                    &   | & \Bool                               & \text{type of booleans} \\\\
                    &   | & \true ~|~ \false                    & \text{boolean values} \\\\
                    &   | & \Pi{x:\ttype_1}{\ttype_2}           & \text{dependent function type} \\\\
                    &   | & \lam{x:\ttype}{\texpr}              & \text{functions} \\\\
                    &   | & \Record{l:\vtype_1, \vtype_2}       & \text{record type extension} \\\\
                    &   | & \Record{}                           & \text{empty record type} \\\\
                    &   | & \record{l=\vexpr_1, \vexpr_2}       & \text{record extension} \\\\
                    &   | & \record{}                           & \text{empty record} \\\\
    \\\\
\end{array}
\\]

Note the absence of the type annotations and explicit substitutions that were
present in the raw and elaborated terms.

[whnf-wikipedia]: https://en.wikipedia.org/wiki/Lambda_calculus_definition#Weak_head_normal_form

### Contexts

As we typecheck terms, we'll be passing over bindings like lambdas and pi types.
Contexts allow us to keep track of the bound parameters,
even though we don't know the exact values these will eventually take during normalization.

\\[
\begin{array}{rrll}
    \Gamma  & ::= & \epsilon              & \text{the empty context} \\\\
            &   | & \Gamma,x:\vtype       & \text{context extended with a type claim} \\\\
            &   | & \Gamma,x=\texpr       & \text{context extended with a definition} \\\\
\end{array}
\\]

## Semantics

We take a _bidirectional_ approach to type checking, splitting it into two
phases: type checking and type inference. This makes the flow of information
through the type checker clear and relatively easy to reason about.
Normalization happens after inference, and before types are fed back in to be
used during type checkiong.

With that in mind, the next sections will describe the following judgements:

| name                                      | notation                                                  | inputs                                            | outputs                    |
|-------------------------------------------|-----------------------------------------------------------|---------------------------------------------------|----------------------------|
| [normalization](#normalization)           | \\(\eval{ \Gamma }{ \texpr }{ \vexpr }\\)                 | \\(\Gamma\\), \\(\rexpr\\)                        | \\(\vexpr\\)               |
| [push substitutions](#push-substitutions) | \\(\pushSubsts{ \Gamma }{\vexpr}{x}{\texpr}{ \vexpr' }\\) | \\(\Gamma\\), \\(\vexpr\\), \\(x\\), \\(\texpr\\) | \\(\vexpr'\\)              |
| [type checking](#type-checking)           | \\(\check{ \Gamma }{ \rexpr }{ \vtype }{ \texpr }\\)      | \\(\Gamma\\), \\(\rexpr\\), \\(\vtype\\)          | \\(\texpr\\)               |
| [type inference](#type-inference)         | \\(\infer{ \Gamma }{ \rexpr }{ \vtype }{ \texpr }\\)      | \\(\Gamma\\), \\(\rexpr\\)                        | \\(\vtype\\), \\(\texpr\\) |

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
    \rule{E-BOOL}{}{
        \eval{ \Gamma }{ \Bool }{ \Bool }
    }
    \\\\[2em]
    \rule{E-TRUE}{}{
        \eval{ \Gamma }{ \true }{ \true }
    }
    \\\\[2em]
    \rule{E-FALSE}{}{
        \eval{ \Gamma }{ \false }{ \false }
    }
    \\\\[2em]
    \rule{E-VAR}{
        x=\texpr \notin \Gamma
    }{
        \eval{ \Gamma }{ x }{ x }
    }
    \\\\[2em]
    \rule{E-VAR-DEF}{
        x=\texpr \in \Gamma
        \qquad
        \eval{ \Gamma }{ \texpr }{ \vexpr }
    }{
        \eval{ \Gamma }{ x }{ \vexpr }
    }
    \\\\[2em]
    \rule{E-PI}{}{
        \eval{ \Gamma }{ \Pi{x:\ttype_1}{\ttype_2} }{ \Pi{x:\ttype_1}{\ttype_2} }
    }
    \\\\[2em]
    \rule{E-LAM}{}{
        \eval{ \Gamma }{ \lam{x:\ttype}{\texpr} }{ \lam{x:\ttype}{\texpr} }
    }
    \\\\[2em]
    \rule{E-APP}{
        \eval{ \Gamma }{ \texpr_1 }{ \lam{x:\vtype_1}{\vexpr_1} }
        \qquad
        \eval{ \Gamma }{ \texpr_2 }{ \vexpr_2 }
        \qquad
        \eval{ \Gamma }{ \subst{\vexpr_1}{x}{\vexpr_2} }{ \vexpr_3 }
    }{
        \eval{ \Gamma }{ \app{\texpr_1}{\texpr_2} }{ \vexpr_3 }
    }
    \\\\[2em]
    \rule{E-SUBST}{
        \eval{ \Gamma, x=\texpr_2 }{ \texpr_1 }{ \vexpr }
        \qquad
        \pushSubsts{ \Gamma }{ \vexpr }{x}{\texpr_2}{ \vexpr' }
    }{
        \eval{ \Gamma }{ \subst{\texpr_1}{x}{\texpr_2} }{ \vexpr' }
    }
    \\\\[2em]
    \rule{E-IF}{
        \eval{ \Gamma }{ \nexpr }{ \nexpr' }
    }{
        \eval{ \Gamma }{ \ifte{\nexpr}{\texpr_1}{\texpr_2} }{ \ifte{\nexpr'}{\texpr_1}{\texpr_2} }
    }
    \\\\[2em]
    \rule{E-IF-TRUE}{
        \eval{ \Gamma }{ \nexpr }{ \true }
        \qquad
        \eval{ \Gamma }{ \texpr_1 }{ \vexpr_1 }
    }{
        \eval{ \Gamma }{ \ifte{\nexpr}{\texpr_1}{\texpr_2} }{ \vexpr_1 }
    }
    \\\\[2em]
    \rule{E-IF-FALSE}{
        \eval{ \Gamma }{ \nexpr }{ \false }
        \qquad
        \eval{ \Gamma }{ \texpr_2 }{ \vexpr_2 }
    }{
        \eval{ \Gamma }{ \ifte{\nexpr}{\texpr_1}{\texpr_2} }{ \vexpr_2 }
    }
    \\\\[2em]
    \rule{E-RECORD-TYPE}{
        \eval{ \Gamma }{ \ttype_1 }{ \vtype_1 }
        \qquad
        \eval{ \Gamma }{ \ttype_2 }{ \vtype_2 }
    }{
        \eval{ \Gamma }{ \Record{l:\ttype_1, \ttype_2} }{ \Record{l:\vtype_1, \vtype_2} }
    }
    \\\\[2em]
    \rule{E-RECORD}{
        \eval{ \Gamma }{ \texpr_1 }{ \vexpr_1 }
        \qquad
        \eval{ \Gamma }{ \texpr_2 }{ \vexpr_2 }
    }{
        \eval{ \Gamma }{ \record{l=\texpr_1, \texpr_2} }{ \record{l=\vexpr_1, \vexpr_2} }
    }
    \\\\[2em]
    \rule{E-EMPTY-RECORD-TYPE}{}{
        \eval{ \Gamma }{ \Record{} }{ \Record{} }
    }
    \\\\[2em]
    \rule{E-EMPTY-RECORD}{}{
        \eval{ \Gamma }{ \record{} }{ \record{} }
    }
    \\\\[2em]
    \rule{E-PROJ}{
        \eval{ \Gamma }{ \texpr_1 }{ \vexpr_1 }
        \qquad
        \vexpr_2 = \field(x, \vexpr_1)
    }{
        \eval{ \Gamma }{ \texpr_1.x }{ \vexpr_2 }
    }
    \\\\[2em]
\end{array}
\\]

We define \\(\field(-,-)\\) like so:

\\[
\begin{array}{lrll}
    \field(x, \record{y = \vexpr_1, \vexpr_2}) & = & \vexpr_1 & \text{if} ~ x \equiv y \\\\
    \field(x, \record{y = \vexpr_1, \vexpr_2}) & = & \field(x, \vexpr_2) \\\\
\end{array}
\\]

### Push substitutions

We define substitution explicitly here. The main purpose of these judgements is
to 'push' the explicit substitutions into constructors as we normalize:

\\[
\boxed{
    \pushSubsts{ \Gamma }{ \vexpr }{ x }{ \texpr }{ \vexpr' }
}
\\\\[2em]
\begin{array}{cl}
    \rule{PS-LAM}{}{
        \pushSubsts{ \Gamma }{ \lam{y:\ttype}{\texpr_1} }{ x }{ \texpr_2 }
            { \lam{y:\ttype}{\subst{\texpr_1}{x}{\texpr_2}} }
    }
    \\\\[2em]
    \rule{PS-APP}{}{
        \pushSubsts{ \Gamma }{ (\app{\nexpr}{\texpr_1}) }{ x }{ \texpr_2 }
            { \app{\nexpr}{(\subst{\texpr_1}{x}{\texpr_2})} }
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
        \eval{ \Gamma }{ \ttype_1 }{ \vtype_1 }
        \qquad
        \eval{ \Gamma }{ \ttype_2 }{ \vtype_2 }
        \qquad
        \check{ \Gamma, x:\vtype_1 }{ \rexpr }{ \vtype_2 }{ \texpr }
    }{
        \check{ \Gamma }{ \lam{x}{\rexpr} }{ \Pi{x:\ttype_1}{\ttype_2} }{ \lam{x:\vtype_1}{\texpr} }
    }
    \\\\[2em]
    \rule{C-IF}{
        \check{ \Gamma }{ \rexpr_1 }{ \Bool }{ \texpr_1 }
        \qquad
        \check{ \Gamma }{ \rexpr_2 }{ \vtype }{ \texpr_2 }
        \qquad
        \check{ \Gamma }{ \rexpr_3 }{ \vtype }{ \texpr_3 }
    }{
        \check{ \Gamma }{ \ifte{\rexpr_1}{\rexpr_2}{\rexpr_3} }{ \vtype }{ \ifte{\texpr_1}{\texpr_2}{\texpr_3} }
    }
    \\\\[2em]
    \rule{C-RECORD}{
        l_1 \equiv l_2
        \qquad
        \check{ \Gamma }{ \rexpr_1 }{ \vtype_1 }{ \texpr_1 }
        \qquad
        \check{ \Gamma }{ \rexpr_2 }{ \vtype_2 }{ \texpr_2 }
    }{
        \check{ \Gamma }{ \record{l_1=\rexpr_1, \rexpr_2} }{ \Record{l_2:\vtype_1, \vtype_2} }{ \record{l_1=\texpr_1, \texpr_2} }
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
    \rule{I-BOOL}{}{
        \infer{ \Gamma }{ \Bool }{ \Type_0 }{ \Bool }
    }
    \\\\[2em]
    \rule{I-TRUE}{}{
        \infer{ \Gamma }{ \true }{ \Bool }{ \true }
    }
    \\\\[2em]
    \rule{I-FALSE}{}{
        \infer{ \Gamma }{ \false }{ \Bool }{ \false }
    }
    \\\\[2em]
    \rule{I-VAR}{
        x:\vtype \in \Gamma
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
        \infer{ \Gamma, x:\vtype_1 }{ \rexpr }{ \vtype_2 }{ \texpr }
    }{
        \infer{ \Gamma }{ \lam{x:\rtype}{\rexpr} }{ \Pi{x:\ttype}{\vtype_2} }{ \lam{x:\ttype}{\texpr} }
    }
    \\\\[2em]
    \rule{I-APP}{
        \infer{ \Gamma }{ \rexpr_1 }{ \Pi{x:\vtype_1}{\vtype_2} }{ \texpr_1 }
        \qquad
        \check{ \Gamma }{ \rexpr_2 }{ \vtype_1 }{ \texpr_2 }
        \qquad
        \eval{ \Gamma }{ \subst{\vtype_2}{x}{\texpr_2} }{ \vtype_3 }
    }{
        \infer{ \Gamma }{ \app{\rexpr_1}{\rexpr_2} }{ \vtype_3 }{ \app{\texpr_1}{\texpr_2} }
    }
    \\\\[2em]
    \rule{I-RECORD-TYPE}{
        \infer{ \Gamma }{ \rtype_1 }{ \Type_i }{ \ttype_1 }
        \qquad
        \infer{ \Gamma }{ \rtype_2 }{ \Type_j }{ \ttype_2 }
    }{
        \infer{ \Gamma }{ \Record{l:\rtype_1, \rtype_2} }{ \Type_{\max(i,j)} }{ \Record{l:\ttype_1, \ttype_2} }
    }
    \\\\[2em]
    \rule{I-RECORD}{
        \infer{ \Gamma }{ \rexpr_1 }{ \vtype_1 }{ \texpr_1 }
        \qquad
        \infer{ \Gamma }{ \rexpr_2 }{ \vtype_2 }{ \texpr_2 }
    }{
        \infer{ \Gamma }{ \record{l=\rexpr_1, \rexpr_2} }{ \Record{l:\vtype_1, \vtype_2} }{ \record{l=\texpr_1, \texpr_2} }
    }
    \\\\[2em]
    \rule{I-EMPTY-RECORD-TYPE}{}{
        \infer{ \Gamma }{ \Record{} }{ \Type_0 }{ \Record{} }
    }
    \\\\[2em]
    \rule{I-EMPTY-RECORD}{}{
        \infer{ \Gamma }{ \record{} }{ \Record{} }{ \record{} }
    }
    \\\\[2em]
    \rule{I-PROJ}{
        \infer{ \Gamma }{ \rexpr }{ \vtype_1 }{ \texpr }
        \qquad
        \vtype_2 = \fieldty(x, \vtype_1)
    }{
        \infer{ \Gamma }{ \rexpr.x }{ \vtype_2 }{ \texpr.x }
    }
    \\\\[2em]
\end{array}
\\]

We define \\(\fieldty(-,-)\\) like so:

\\[
\begin{array}{lrll}
    \fieldty(x, \Record{y : \vexpr_1, \vexpr_2}) & = & \vexpr_1 & \text{if} ~ x \equiv y \\\\
    \fieldty(x, \Record{y : \vexpr_1, \vexpr_2}) & = & \field(x, \vexpr_2) \\\\
\end{array}
\\]

## Related work

- LambdaPi
- PiSigma
- UTT
- Agda

See references page

> TODO: clean this up

## Future work

- formalization in proof system
- additional features
    - dependent records
    - cumulative universes
    - linear types and erasure
    - coeffects/effects
    - memory size analysis
    - inductive data types
    - performance optimizations

> TODO: clean this up
