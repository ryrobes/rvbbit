# RⱯBBIT DYNAMIC DATASCAPES
## Reactive Data Board & Visual Flow Platform

As a long-time dashboard builder, data engineer, and UI hacker, I've always wanted something in-between Tableau & building bespoke web data products to ship answers to my users. The tools were too rigid at times, and building everything from scratch can be tiresome. This is the eternal push/pull of DE and SWE approaches, as many who work in BI can attest to.

So I began to experiment with how you can have a platform that, to paraphrase Alan Kay, "Simple things should be simple, complex things should be possible". I'll add that I wanted 'beautiful' to be possible as well.

### A dynamic, flexible, visual platform of reactive parameters - and virtually *everything* is a parameter - could be a dataset, a row, a clicked value, a CSS map, or a single Hex color.

RVBBIT is a next-generation hypermedia platform for interactive data visualization & analysis, ingeniously synthesizing influences from HyperCard, Smalltalk, and Lisp/Clojure. At its core is the 'reactor,' an advanced system of reactive pathways that orchestrates a dynamic interplay of data and functionality across the entire platform.

Built on a Clojure foundation, RVBBIT organizes functionality into interactive 'Cards' within 'Boards' and 'Decks'. These components communicate through a highly granular system of Clover/Reactor pathways, which manifest to users as familiar reactive parameters. This approach echoes Smalltalk's object communication model while leveraging modern distributed computing concepts.
The Clover DSL enables the definition of these reactive pathways and facilitates complex inter-Card interactions. This creates a live, REPL-driven environment where changes in one part of the system can trigger cascading updates through precisely defined pathways, enabling the creation of deeply interconnected, reactive data experiences.
RVBBIT offers a unique blend of visual intuition and coding power, catering to users across all skill levels. It supports rapid prototyping and exploratory analysis, allowing for the development of sophisticated, non-linear data applications. By reimagining classic interactive programming concepts through the lens of modern data needs and distributed systems principles, RVBBIT pushes the boundaries of what's possible in data visualization and analysis.

RVBBIT draws significant inspiration from pioneering systems like Smalltalk, HyperCard, & the breadcrumbs left by Bret Victor. Like Smalltalk, it embraces the idea of a fully malleable, live programming environment where everything is an object and can be inspected and modified. From HyperCard, the vision of an intuitive, visual programming paradigm that empowers users to create interactive, linked information systems. This blend of influences results in a platform that combines the power of a modern programming environment with the accessibility and rapid prototyping capabilities reminiscent of these groundbreaking tools - yet hyper focused on building data products such as dashboards, data science views, and interactive explorations.

### Overall UI Approach:

- Help you create in broad strokes:
   - Code generation
   - Drag & drop operations
   - Value scrubbing
- Let you tweak & edit the code to produce the desired result
- No hidden "magic", no step-by-step 'wizards' that create uneditable artifacts

Low bars and High ceilings.

## Ladder of Abstraction

In terms of Bret Victor's 'ladder of abstraction' concept:

| Level | Description |
|-------|-------------|
| Low   | Context-aware drag & drop creates basic blocks by writing queries and view DSL (query, visualize, filter, link) |
| Medium| Blocks can be tweaked to fit needs (small changes, CSS, options, scrubbers) |
| High  | Raw code to produce DSL or new features added wholesale via kits/solvers/flows |

This allows users to build up mastery and have all these components work together - a DnD created SQL rowset can control a bespoke REPL code block and vice versa. They all speak the same core language = reactive parameters.

## Clojure and RVBBIT

While RVBBIT is very much a "Clojure platform", knowing Clojure is not a hard requirement. You can build an entire dashboard with only drag & drop and a SQL database - but its full flexibility is unlocked with some Clojure knowledge.

## Main Systems

RVBBIT includes these main systems that we consider a base level of primitives:

1. **The Canvas**: A composable & extremely customizable surface (reactive themes, parameters, tabs, etc)
2. **Blocks**: Arbitrary nREPL connections, SQL, views, and custom user-space defined functions
3. **History**: Visually browse previous block edits and swap between them to revert, explore
4. **Reactors**: An extensive system of reactive parameters / param-keypaths that act seamlessly in a pub/sub fashion
5. **Snapshots**: Saved state of parameters as a special named tab and swap between them (storyboards, slides, etc)
6. **Signals**: Boolean triggers defined in Honey-SQL DSL fragments - arbitrary values or function returns
7. **Solvers**: Arbitrary functions that can be triggered by signals or by user UI actions
8. **Flows**: A complete Flow Base Programming node builder system
9. **Kits**: User-space plug-in definitions that turn an external nREPL into a micro-service
10. **AI-Mutable Design**: Fully programmable, literally built for dynamic mutations
11. **Shared Everything**: Every client session has access to every other client sessions objects as parameters

## SQL Specific Features

- User-space configurable viz-reco system
- Drag and drop SQL operations
- Cache table cross joins

## REPL Specific Features

- Visualization of common CLJ data types including visual GET-INs via dragging
- Realtime console output
- Console as data

## Demos

(Placeholder for gifs and videos demonstrating various features)

## Caveats

- Use Englebarts beautiful invention
- Some UI jank
- Limited SQL DB support ATM
- No concept of users / auth yet

---

For more information, please check out our individual demo pages and full videos showcasing RVBBIT as:
- A SQL dashboard
- A canvas of REPLs
- Dynamic parameters
- A flow runner with UI bindings
- AI assist using a context-aware Fabric pattern

