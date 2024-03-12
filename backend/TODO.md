

semantic refactor coming! 

"wine-bars"
    - sniff tests
        - what are we looking for?
    - taste tests
        - what do we like about it?
    - palette (set of attributes?)
        - what does it mean all together?
        - generates results
    - vintage

    - dashboard might be a "flight"? lol

    SOUMOUILE IS THE NAME OF THE RECOMMENDER SERVICE?

    Lets give it a sniffy sniff.

    Lets give it a whirl.

    I'm getting a bit of high cardinality on the nose here...

    WINE LIBrary THEMED INTRO VIDEO? "WELCOME TO WINE LIBRARY, I AM YOUR HOST RYAN RO-BUH-TIE!"

**********************************************************************************************************************
TODO list, addendum - 10/31/22 - ryrobes 
TONIGHT - hopefully.

//- get sniffer FIELD rules working 100% 
//- get sniffer TABLE rules working 100%
//- get sniffer WHEN vector-clauses working [:= :db-type "mysql"] - recursive attrib-bools will come later hopefully.
//    - needs to be passed a context - just vector-key values (plus connection table details - see below)
//    - if we can keep it to SQL that can be run to get true/false, maybe itll help with the nesting later?
//    - since is-mysql? attrib is just a shorthand for [:= :db-type "mysql"], which is just shorthand for {:select 1 from connection row where db-type = 'mysql'}
//        - so we def need a mysql table for connection info - but only the conn info gathered from the driver itself. so itll always be a string, even if we conn w a map
?- allow omitting the body of the select and just ASSUME [select x from table] - lets table this. NAH.

//- single values are NOT getting inserted anywhere?

//- doesnt look like :top-10-values is working? also, would be great to datagrip this shit....
//- cleaned up a bunch of shit. added run_id
//- need a generalized field-type for logic: string, date, integer, float 

//- start building attributes - use sniff :when as a model? same thing, but diff SQL context... 
//- can use similar logic to sniffer WHEN rules, but w a different target of replacement values?

//- sniff tests - count distinct. having :field in subquery showed up as a TABLE sniff and not a field one?

- rename everything to our wine / soumier theme?

- need to deal with field vs table attributes (is that even a thing?)
    - maybe {:is-big-table? [:>= 1000000 :total-rows]}
    - maybe if they reference a table sniff, we only run them on :* ?

??- need to figure out multiple-value attrib / taste tests??? IF
??    - another outer join on an aliased table?
??    - [:= :sample-rows.my-field "something"] if it exists at all?
??    - or maybe multiple row "samples" are only for visual table inspection / UI reasons?
??    - I honestly cant think of a reason for them that I couldn't do with a sniff test SQL stmt
??    - maybe they are purely for showing fast samples in a data portal or something

//- field-vectors needs table_cat / db-name added to connections and everything that reads field-vectors

//- add table schema to all SQL queries... unless nil 

//- test an extra data source or two... postgres, clickhouse, mysql

//- need to be able to target only certain schemas... else we get all the system-db shit. no bueno.

//- insert derived fields into attributes... make sure they dont overwrite old vals

//- should be able to limit table and schema search per datasources {:tables-containing? [""] :schemas []} ? use SQL? lol  [:and [:in :db-schema []] [:<> :table-name "fart"]] hehe

//- put a key_hash in other tables
//    - double check the hash_key logic to be unique at the field level

??- put connection_id in errors if possible 
??    - its only really needed where we connect to target-db... which is very few lines

//- fix all the errors 
//    - mysql select * from database.schema.table ? or use database; select ... ?

//- mysql cataalog and schema are mixed up? need to USE DATABASE; and prefix properly? shit is weird 

//- change the output sql mini report to group by connection with counts for each type by rows - maybe distinct type and attribe too, just to make sure nothing weird is happening

//- we seem to be missing a lot of tests and attributes for having so many databases?

//- add key_hash field to attribs for easier orphan cleanup later...

//- we should probably have a "fields" table with just the fields-vector rows in it?
//    - add connection_id and run_id 

//- looks-like-date? LIKE literal "-" are being squashed to _ _ _ fuck. need to be smarter with the string fuckery...

//- do derived fields...!

//- found fields will be sent back through the sniff loop

//- can't process a field until it has been through full attribute step
//    - duh, just do it after attribute. lol


Is there some rule for academic projects to limit their scope to such a small little terrarium of the world that they might be interesting, but unusable for the practical world without revisiting many of their main assumptions?
Software that only exists to write a paper.


??- take 45 mins to look at a HUMBLE UI experiment and how odoyle rules might work ?

??- look at OZ vega-lite shapes 

??- look at voyager2 for multilple-field implementations 

ugh. why bother. frustrated.

{maybe this is just part of vizyard or wine-bars-ui and the lib stays pure as a lib - i.e. BYO-APIServer}
- find lightweight API / websocket server inside wine-bars ? 
    one that can be stopped and started via REPL

    if so... push wine-bars to GH so we can use it as a git dep... but would need cleaning up, etc. renaming things. moving fns around...



- at this point we kinda need a UI to do non annoying work...  

- write some "query" fns to pull the recos, samples, etc out - kind of like the API endpoints you would need for an app - but fns for them... (base library reqs this)










    ** would be super cool if the UI could "EXPORT" a configured dashboard as a simple standalone clojure project with oz, hiccup, and JDBC calls (re-frame and shadow also?). 
        - its going to be open source, so this isnt' really needed I dont think? but maybe as part of the web UI... export as SOMETHING

    ** or we seperate it into 2 (or 3 libs?) - the base library, the API endpoints (small), the web-admin-UI that ALSO makes/exports Oz dashboards... ?
        - or the last 2 are just a FE with web sockets?

    ** would be very cool to provide a csv file url as the JDBC conn and it would get file - insert it into a SQLite table and then analyze it as its own JDBC conn
        - great for docs and demos. pull down random web .csvs etc
        - would ALSO be cool to do the same if it was provided a clj rowset... (rabbit use here)



 - just the library = wine-bars
 - web socket server + UI = viz-yard 







**********************************************************************************************************************
TODO list - 10/30/22 - ryrobes 

meta data / table features pull:

    - derived fields (simple and common generated SQL calcs)
        - date dims
            - if field is a date / or a string that LOOKS like a date
                - get year(field), month(field), week(field), year/month(field), year/week(field)
        - range bucketing
            - if the date is a low cardinality integer
                - look at the min/max (distribution ideally?)
                - generate some case statements to bucket them in a derived string dimension

    - lets try to insert everything into a SQL db - duck or sqlite?
        - rvbbit-backend.db - rules get saved in sniff-tests.edn, viz-suggest.edn, etc?
        - metadata, sniffs, sample, rows, query/viz pre-aggs
        - will make re-aggs / post-aggs, external joins a bit easier as well? in future. csvs, etc.
            - maybe we use DuckDB or some other analytical / columnar DB for that...

    - generalize the data types - string, int, float, date, maybe date, boolean - can we use user-attribs here?

    - flag if dim or measure - should be a sniff test?

    - move sniff tests to honeysql instead of strings


    - create a [sniff-test-map] data structure that sniffs can be read off of

        :distinct-values {:sql-map    {:select-distinct [:field] :from [:table]} ;; [:count :distinct :field]? or [:raw "count(distinct field)"]?
                          :when       [:= db-type "mysql"] ;; optional  - or :mysql? if we are recurring...
                          :fetch-one? false}
        :max-val         {:sql-map    {:select [:max :field] :from [:table]}
                          :when       [not-string?]
                          :fetch-one? true}

        :total-rows     {:sql-map    {:select [:count :*] :from [:table]}      ;; a 'table sniff', maybe ones that don't use :field...
                          :when       [not-string?]
                          :fetch-one? false}
        :sample-rows     {:sql-map    {:select [:*] :from [:table] :limit 200} ;; a 'table sniff'
                          :when       [not-string?]
                          :fetch-one? false}


    - create a [field-attribute-map] data structure that can be read to create boolean attribs for tests

        :measure?               [:or [:= :column-type "number"] [:= :column-type "float"]]
        :dimension?             [:!= :column-type "number"]
        :low-cardinality?       [:>= :distinct-values 30]
        :very-low-cardinality?  [[:<= :distinct-values 15] [:> :distinct-values 3]]


    - create a [derived-fields-map] data structure - will create new fields by reading and feed back to step 1                        

        :by-year           {:is [:date?]
                            :is-not [] ;; optional 
                            :sql [:year :field]}
        :range-bucket      {:is [:integer? [:or [:low-cardinality? :medium-cardinality?]]]
                            :sql [:case 
                                    {:when [:= :field [:~ :max-val]] :then "whoa"}
                                    {:when [:<= :field [:~ :max-val]] :then "ok then"}
                                  :else "what!]}

        :gross-revenue     {:is [[:= :table "mytable]] ;; this could be a metric? globally calculated field...? we're in weird territory?
                            :sql [:raw "sum(revenue) + sum(net_revenue)"]}

    - create a [viz-suggest-map] data structure that can be read to create the viz queries

        :simple-horizontal-bar  {:x {:is     [:dimension? :low-cardinality? :string?]
                                     :is-not [:date? :datetime?]}
                                 :y [:measure?]}

                                 ;; keys should be arbitrary so user can do whatever w the 
                                 ;; field sugg + template (:tooltip1 :tooltip2 :label1 etc)
                                 ;; but my base defs will mostly be :y :x :color :size :label :row :col etc
                                 ;; think about voyager2...

                                 :base-usefulness 1 ;; human curation via 

                                 :vega-lite {} ;; etc? or... 
                                 :viz-map {} ;; and have :vega-lite be a wrapping key around :simple-horizonal-bar ?

        (embed a small vega-lite shape for each that shows where to put the :x :y etc elsewhere?)

        then for vega-lite layers... we can suggest any thing with a matching x or y axis.. etc



    ## FUTURE...

    - since were using this for sawmill - lets make sure we can have an ADDED layer of abstraction to use arbitrary viz libraries

    a "unified data viz library wrapper?"

    - viz-library-map ?

    [:vega-lite {:x x :y x :type "bar}] - since we're a backend lib, we just output the config map needed for rendering...
    [:plotly {:x x :y x :type "bar}]
    [:nivo {:x x :y x :type "bar}]


    rvbbit-backend/dendrochronology

        - HumbleUI app to upvote/downvote suggestions / write them in semi-realtime
        - would also be cool for it to run ad-hoc viz configs as well as SQL queries
            - crossover with LarsQL...?
    

            
