# RⱯBBIT DYNAMIC DATASCAPES
## Reactive Data Board & Visual Flow Platform

[ Some kind of quote or dictionary term? ] creativity is working on something for longer than is reasonable?
composability def. compostion?
The dictionary definition of "composability" is "the quality of being composed or assembled."
The dictionary definition of "composition" is "the act of combining parts into a whole."
The dictionary definition of "feedback loops" is "a loop that allows information to be passed back and forth between parts of a system."
Visual Composition is the act of combining parts into a whole. In Hypercard, you could compose a board by dragging and dropping cards onto a board. In RVBBIT, you can compose a board by writing code.

In a world drowning in data, we don't need more dashboards. We need data landscapes we can explore.

"If you want to build a ship, don't drum up people to collect wood and don't assign them tasks and work, but rather teach them to long for the endless immensity of the sea." — Antoine de Saint-Exupéry

composability (n): the art of creating complex systems from simple, reusable parts.
feedback loop (n): a cycle where output influences input, creating a self-reinforcing system.

Data Visualization Alignment Chart:
Lawful Good: Excel | Chaotic Good: D3.js
Lawful Neutral: Tableau | True Neutral: matplotlib
Lawful Evil: PowerBI | Chaotic Evil: RⱯBBIT


Why?

As a long-time dashboard builder, data engineer, and UI hacker, I've always wanted something in-between Tableau & building bespoke web data products to ship answers to my users. The tools were too rigid at times, and building everything from scratch can be tiresome. The eternal push/pull of DE and SWE approaches, as many who work in BI can attest to. How could I have the flexibility & re-usability of code, but the compositional freedom & direct manipulation of a builder tool? 

I don't want my creativity limited by tool nor timeline. 

To paraphrase Alan Kay, "Simple things should be simple, complex things should be possible". I wanted 'beautiful' to be possible as well.

A data platform should feel more like using a game engine than using an "app". 

What is it?

### A highly dynamic & flexible platform for composing interactive data boards, a data workspace canvas, & flow-based programming system

You could even call it a "non-linear anti-notebook that puts composition and presentation front & center rather than an afterthought". Heresy!   
[Notebook enjoyers scroll their procedural top-to-bottom rectangular fists up and down at me angrily]

Did you say "Data boards"? Yes. "Dashboard" is very 1990s. 
More Minority Report, less Office Space. Baby steps.

RVBBIT is a highly expressive hypermedia platform for interactive data visualization & analysis, synthesizing influences from HyperCard, Smalltalk, and Lisp/Clojure. At its core is the 'reactor,' an advanced system of reactive pathways that orchestrates a dynamic interplay of data and functionality across the entire platform.

RVBBIT draws significant inspiration from pioneering systems like Smalltalk, HyperCard, & the breadcrumbs left by Bret Victor. Like Smalltalk, it embraces the idea of a fully malleable, live programming environment where everything is an object and can be inspected and modified. From HyperCard, the vision of an intuitive, visual programming paradigm that empowers users to create interactive, linked information systems. This blend of influences results in a platform that combines the power of a modern programming environment with the accessibility and rapid prototyping capabilities reminiscent of these groundbreaking tools - yet hyper focused on building data products such as dashboards, data science views, and interactive explorations.

## "Low bar, High ceiling"

The skill level of users varies, but we all still need to get sh*t done. Answers need to be found, and data value needs to be delivered to the people.

RVBBIT helps bridge these gaps in it's overall approach to building - it helps you create in board strokes by generating "code" (SQL, view DSL, Clojure, etc) in it's cards from simple drag and drop operations. 
The user can then modify this working code, or use other UI like value scrubbers to mutate and see the changes in a quick feedback loop cycle - this builds understanding.
No hidden "magic", no step-by-step 'wizards' that create un-editable artifacts or configurations that are opaque.

## Ladder of Abstraction

In terms of Bret Victor's 'ladder of abstraction' concept:

| Level | Description |
|-------|-------------|
| Low   | Context-aware drag & drop creates basic blocks by writing queries and view DSL (query, visualize, filter, link) |
| Medium| Blocks can be tweaked to fit needs (small changes, CSS, options, scrubbers) |
| High  | Raw code to produce DSL or new features added wholesale via kits/solvers/flows |

This allows users to build up mastery as all components work together - a DnD created SQL rowset can control a bespoke REPL code block and vice versa. They all speak the same core language = reactive parameters.

This also has a 3rd order effect - due to RVBBIT's open systems - advanced users can extend the functionality of the platform - delivering new packaged behavior & bespoke artifacts in their own creations.

## Clojure based 
### Data is Code, Code is Data.

While RVBBIT is very much a "Clojure platform" and a full-blown nREPL client, knowing Clojure is *not* a hard requirement. You can build an entire dashboard with only drag & drop and a SQL database - but its full flexibility is unlocked with some Clojure knowledge.

## SQL Specific Features

- Drag and drop SQL operations on a "cutting board" canvas
	+ Taking a "reverse" approach to query building - users can start with full table views and then slice and dice them down to what they need as opposed to building field by field.
	+ Easy parameter filtering, pivots, joins, etc. Reactive data triggering SQL queries has never been this flexible and powerful
- User-space configurable viz-reco system
	+ create programmable templates with a straightforward SQL syntax, custom attributes, and freeform views - these can then be recommended to users if the data shape fits the view definitions.
- Cache table cross joins
	+ A unified SQL cache layer allows joining of arbitrary queries, no matter what database they come from (or if they came from a Clojure REPL value, any other place)

## Clojure REPL Specific Features

- Connect to any remote nREPL, or use the built in one
- "Visualization" of common CLJ data types including visual GET-INs via dragging
- Realtime console output, console as data

## Reactive Parameters

- All cards on the front-end can utilize an entire system of reactive parameters, both from the front-end as well as the back-end. Scheduled processes can push "subscribed" values to dashboards in realtime, and front-end parameters can change back-end values. Clients are subscribed to a parameter simply by using it - the server keeps all the clients in sync.
- An entire library of "addressable" parameters
	+ Every Card, Stack, Board in any saved Deck
	+ Any (live) Card or parameter from any other client(!)
	+ The value of any step in a Flow 
- Are universal across all objects
	+ A custom view click action can trigger a REPL execution, which then can trigger a SQL query, which then can cascade into a chart or view update, or mutate the users canvas in some way, and so on.

## reusables ?
use a query or a view from another saved deck - it gets updated, the changes stream into your deck, live.    

## Configurable Card "Runners"

Besides the built-in runners of SQL queries and Views (UI) - the combination of arbitrary parametrized code + an integrated Flow system enables all kinds of functionality that can be packaged up as a new runners and available for users to build with. Essentially creating new Card types with new functionality.
- Examples
	+ A flow that hits OpenAI's DALLE3 API, with a prompt, downloads the image, saves it as a JPEG, adds Exif data, and then displays it in a generated HTML view. All the user knows is that there is an :image-generator card - they type in text (the card's 'source') and the output is an image to be used on their canvas
	+ A custom nREPL connection that executes some arbitary code or text and returns some specific output - like a shell command, or to a Python interpreter, an R calculation - or perhaps an nREPL across the office that has special libraries and hardware to process very specific tasks. All wrapped up in a friendly interface. Just another "card" on your data board.

## Flow Based Programming Builder / Runner
	also runstreams. flows can be pinned to a data board
	completely integrated
	
## Snapshots

## Signals & Solvers

## Metrics & KPIs
coming soon. 

## Dynamic Theme System
Did I mention reactive parameters? Yes, the theme of the UI is itself a set of reactive parameters. CSS maps everywhere. Specific card overrides, global defaults, conditional formatting. Data-driven dynamic theme changes? Yes.

## The Canvas
minimize cards, pin them to all tabs

## Recursive Composition
Arrange a set of a cards in a board tab - drag that composed tab into another board, it's now it's own single card. Great for composing and organizing groups of cards that share functionality or need to be re-used (ex: a "sidebar menu" for a series of dashboard tabs)

## Kits (plug-ins)

## Card History
A visual undo log of all card changes and their code diffs. Easy to scrub between old versions or even drag them out as new cards.

## Value Scrubbers

## Designed with AI mutation in mind
Due to a unified DSL and Clojure's Data as Code, Code as Data structure - the entire canvas is meant to be freely mutable by any Flow, function, DSL action. Super-prompting + context injection (because user-context is just a set of reactive parameters, of course) can go far.

## **Demos**

(Placeholder for gifs and videos demonstrating various features)
Example public dashboards in read-only mode?

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

