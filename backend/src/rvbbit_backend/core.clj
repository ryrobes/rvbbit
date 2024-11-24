(ns rvbbit-backend.core
  (:require
   ;[rvbbit-backend.xtdb-init :as xtdb-init]
   [clj-http.client :as client]
   [clj-time.coerce :as tcc]
   [clj-time.core :as t]
   [clj-time.format :as f]
  ;;  [clj-time.jdbc]
   [clojure.core.async :as    async :refer [<! <!! >! >!! chan go]]
   [clojure.core.async :refer [<! timeout]]
   [clojure.data :as data]
   [clojure.edn :as edn]
   [clojure.java.io :as io]
   [clojure.java.jdbc :as jdbc]
   [clojure.java.shell :as shell]
   [datalevin.core :as d]
   ;[clojure.math.combinatorics :as combo]
   [rvbbit-backend.pool-party :as ppy]
   [clojure.pprint :as ppt]
   [clojure.set :as cset]
   [clojure.string :as cstr]
   [clojure.walk :as walk]
   [flowmaps.core :as flow]
   [flowmaps.db :as flow-db]

  ;;  [xtdb.node :as xtn]
  ;;  [xtdb.api :as xt]

    ;[flowmaps.examples.simple-flows :as fex]
    ;[flowmaps.web :as flow-web]
   [hikari-cp.core :as hik]
   [nextjournal.beholder :as beholder]
   [ring.adapter.jetty9 :as jetty]
   [rvbbit-backend.shape-rotator :as rota]
   [rvbbit-backend.leaves :as leaves]
   [rvbbit-backend.realms :as realms]
   [rvbbit-backend.h2 :as h2]
   [rvbbit-backend.queue-party  :as qp]
   [rvbbit-backend.assistants :as assistants]
   [rvbbit-backend.config :as config]
   [rvbbit-backend.db :as db]
   [rvbbit-backend.freezepop :as fpop]
   [rvbbit-backend.cruiser :as cruiser]
   ;[rvbbit-backend.embeddings :as em]
   [rvbbit-backend.evaluator :as evl]
   [rvbbit-backend.external :as ext]
   [rvbbit-backend.sql :as    sql
    :refer [pool-create sql-exec sql-query metrics-kpi-db import-db systemh2-db
            system-db cache-db cache-db-memory
            system-reporting-db to-sql]]
   [rvbbit-backend.util :as    ut
    :refer [ne?]]
   [rvbbit-backend.websockets :as wss]
   [shutdown.core :as shutdown]
   [websocket-layer.core      :as wl])
  (:import
   [java.util.concurrent TimeUnit]
   java.nio.file.Files
   java.nio.file.Paths
    ;java.nio.file.attribute.FileTime
   java.nio.file.LinkOption
   java.time.format.DateTimeFormatter
   java.time.ZoneId)
  (:gen-class))

;; =======================================================================================================================================
;; Note to readers: lots of refactoring, namespace, mess cleanup to do, I just came out of a very long "move fast and break things" season
;; Note to readers: lots of refactoring, namespace, mess cleanup to do, I just came out of a very long "move fast and break things" season
;; Note to readers: lots of refactoring, namespace, mess cleanup to do, I just came out of a very long "move fast and break things" season
;; =======================================================================================================================================

;;  ............................................................................................................................................................................
;;  .....................................................................................:-.....................................................................................
;;  .......................................................................................=..................................-.................................................
;;  ........................................................................................*...............=.....:............:................................................
;;  .......................:.:......:..........................................-.............+:.........*....:.....:...........=.......-...............:*@@.....................
;;  ......................:::-:::::::.:.........................................::............=+.........*........:-...........=.......-.............#@@@@=.....................
;;  ..................-:-=++*+*=+-------:.................................:....................-%.............-...=:+.-....:+..-:.................+@@@@@@%#.....................
;;  ...................-%@=...-#@@@%**+==:-:::::.:.:.:.....................:.:...................#=..:.........:...--=.-........=.........:@......#@@@@%@@+.....................
;;  .................::+@=..........:+%@@@%#+=-:::............................:...................*#.:=.....+...:..:+=.:........*.................*.-................==.........
;;  .................::-*@........--=+=:.-#*%@@%#=--..::.......................::..................*@+:-.....+:..:..+++.+....-..=-........:................:....................
;;  ...................-=%@.......:.:-+++**+.-*@*@*==:--.:.......................-:........:........=@@=*.....*:..:.:++-.:...*@=.*.......:+...............::..........:.........
;;  ...........:....::--:=#@+........:-=+*###*=:.=@@++-:::::...................:...-:................=@@#=:.....:-::.-**:-...:@@%#.......::......-.......:-.:.......:-...:......
;;  ............:...:...:-+*@%........:-=+*###%#-=-@@#==::::.......=#-.:............:=:.:.............:@@#:=......+-..**+::..:@@@@:......::..:..-*-.....-=.:.....-:+:..::....-..
;;  ......:::::::..::::.:::=#%@:........:==+*#%#%#=:@@%*+--.......:.%@@+..........-:..:=..-...:........:%@@=+......=-:.#*--...%@@@=....*#:...:-.:.:....=-::.....-:+..=:........:
;;  ::::-*@@@@@@%#%%#*=+=-:=+*#@=.........:-=+*#%##+.=@%*+==-::...::+%@@=.....-#::.:-:..-=...+::....-*...+@@:+.:...---.=#*.:.:..=@*.-::-@@-..:........=*=.....:.+..=-.....-..=+.
;;  *@@@*:--:....:#@*:#@@@%#***%@@-........:-==+*#%#=.@@%*+-:::::-::......:.....%@%=.:-=:.-+:.....::::....:@@+=-:::.*=-.*%+-::::..:.---:%@@..:.::.:::++:+#...=-+.:=:.......*=...
;;  ...-***#@@@@@@@#=--.:#@@@%##+%@=.........:--+*#%%*=:@#*--::..:--:::.:.....:..:::...::-::+=..:::-*=:-:.::@@#=+=-:-*-::%%+-::::-::=-:.+#%:-..*=--=%*=.....=+=.=:=....:-*-.....
;;  ##%%%@@@@@@@@@@@@@@@@%@*..@@@%%@@=.........-.+#%*.+-@@%%*+=:----::........::....:::---:=::==-:::..:--=:::#@@=++=-**--=@#+-:::==-+==-:.:=-::%=%@%%=-:-:-+#.+:--.::--#=.......
;;  *#########*#%%@@@@@@@@@@@@::-@@@@@@#.......:+..%@#:@@@@@%*++-+----::---:....::.:..::=-++:+--==-::..:=-*+::=@@=**+=%#--#@**=-:+%*#+=+=-+*+-=#*@@**==-:=*+++*+-::=+#+.........
;;  .............:--+*#%@@@@@@@@%++@@%@@@@@=..%#=@@%+*%@%=:*@@#*+=-----::--:....:.:::::-----++-=-++=-.::-==+%=--@@++#++@*::%%*#*=+%##*++#**#+**#@@@++*+=+%*##@#+--=%*:..........
;;  ...................=*#%%@@@@@@%:%@*%+..:+%@@@@@@@@%=:..=%@@@%*+=---:::-:........::-====---#*=-=*+=-=--:--*#-=@@#+%*#@*+*@@%###%%%%%%*%%##%@@@#%%#**#@%%@%**+*%*+-:::........
;;  ::--:..................=*#%%%@@@=.:++*#@@@@@@@@@@@%@@@@%%:.*@@%==--=:=:-::-.::-.----===-=---*%+:+##===-==+=%+=@@@*@%@@**@@-*=@@@%@@%-+@@%@@@@#@@%@@@@@@@%##@%##*==.......:#=
;;  ...:=++====-::............:+##%%@%**#%@@@@@@@@@@@@@@@@@@@@%:==@@%++-==--------:-:::--=-++=====*%#=#%#=++==%*%#+%@@@@@@@%%@@-#@@@@@@@-%@@@@@@@@@@%*#*%@@@@@@@%%#%+=-::.:#@@+.
;;  :......:-=**+++-:::..........=*##%%%%@@@@@@@@@@@@@@@@@@@@@@@#@:-%%*+==++:+#*+=---:===-===+++++++#@*=%%**%*=%%@@#@@@@@@@@@%@@@@@@@@#*+=-----+#%%@@@-@@@+:.-%@@@@%#*+-:+%+....
;;  @@@@@#.......:-=---..:--:....:+**#%%%%@@@@@@@@@@@@@@@@@@@@@@@@#.%@#++===:-:#@@@@=---==+=-==+***#**%@#+#@@@@@%%@@@@-%%@@@+#+:..=*@@@@=#@@@@@@==@@%*@@@@#*#@=..-@@@%#=:.:-....
;;  :-=*%@@@@%#*#=...=..........:.:-=*%%@@@@@%%@@@@@@@@@@@@@@@@@@@..@@@#*=+-=..+%@@@@@#-=-:-+%**##%%%%@@@@#+%@@@@@@@@@#:@+@@@@*=@@*%#@@@#+@@@@%@@@@@@@@@@@####+=##*..*@#+-:.....
;;  :.--::-=**#@@@@@*+#@@@@@*=-....+##%%###*-..:#@@@@@@@@@#%@##@@=...@@#**++++---::.....:-=*##*#%@@%%@@@@@@@=*=-=#%@@@@%.%#@@@*..-+@#@@@@@@@@#*@%@@@@@@@%@####++####*.%@*=:.....
;;  .:-:--::-===-=*######%%@@%.....=*#####=.......#@@@@@@#+#%**%*.-*..@@%%#*+++**===-=+++**##%%#@@@@@%*=--+#@@##@@@@@@@@%=@#@@=+:%.+-@%*=*--:.:.::=@@@@@@@***#+*#%##*.@@#+:.....
;;  ..--==-::-=--===-=*###@@@-...:-+*##%##::.%@@*...*%%**:%@@@@*....-.%@@@%**#*++++*#=++=++###@@@=.:*@@@@@@@@@@@%**@%*%@@@@@#*+=.-=..:.-.+........-@@@@@@@==**+%####*.@@#+:-....
;;  ...:-::-=--=-:--==+#%%@@=-.....:**#####:+@@-........=%@@%%#*=..=-+-:-%@@%#+**+===-+###*#%%@@-+@@@@@@@=+#@@@@#*@+-=.:=-..##..=.#*...:.-..:.....%%%.@@@%*##%+%##%%+.@@#+--:...
;;  .:.::----=+=-::-=+**%%@@*.....:=+***###=:@@:......:****@@@@@@#-:@@@@#:.*@@#+*+**+=++++*#%@@@.#%%@@@@@@#=#=:....--.....=+.-%..-.@:.-.::.#..:..+*@+@@@#*####+%%###=.@@#+::....
;;  ....-----==++=-:-+#%@@@@%....:--.:-+#***-:@@-.....*@@@@@%@@@@%%%+@@@@@:%@@%%#***+#******#%@@.#%%@+:......-.......*..=..#@.-%...=@.=.::=.....**+.@@@@@#####*#####+.@@%+=-....
;;  :--====-:=++++==++#@@@+.......-*##%@@@@@@%+:..=@%@@@@@@@@%...@@@*@@@@@..=@%#***##%@%#*#%@@@%.%@@@==........*..=....*...=@@@*@:.+*%..-.+.....*.:@@@*@%#*%##+##@%#-.@@#*=---:.
;;  .::==+*==+++++++++#@@@@#...:-+**%@@@@@@@@@@@@@@@@@@@@@@@@@@*#@@@+@@@@#=+%@@###****##%%%@@@@*:@@@#-=...==....:-..#=.:.-..+@@+*@*:=@:.+:-.......-*@@@%+:=*##+%%###-:@@@#*=--::
;;  .:.=+===+++++****#@@@%......-+*%%@@@@@@@@@@@@@@@@*-*#@@@@@+.:%#==@@@%*..@@####*#:+**%@@@@@@*=*%@#++.......=...+*..++-..:..%@-.#@@@@.%@:.:-...:++@@-@@*=:-*+%%%%#:.:@@@%*+-:-
;;  ::..==+--+*******#@@*.:#....:-+**%%@@@@@@@@@@@@@@@:.......%-@..%@@@#-=-=@@%**#%%++%@@@==%@@++%##-==+=.....+-...*##..*#..-*.:@#.+@@-%@-:..=--..:+.:@@@@#%=--%%%%#:.:.+@@##++=
;;  :::=+*++++****###%@@@@@=......=+*+#%@@@@@@@@@@@@@@-......@@=#-%@#*=-..-@@%%%##@@*-*#%@**.@@=*@@@-+==#+-=...-=..=@@@@-...#%:=@@@*:%.=.+.%@#@@#.=..*@@@@####=-####..+*=.@@@%#*
;;  :-:++++++==**##**##@@@@........::+#%##*%@@@@@@@@@#.........:##*+....:@@@@%%%%%%%@#.=*%==@@@-%%%%=+=..:::*@=..:...=+%@@@%=..++-+::::=%@@@%@@#-%=@@@@@@%####+%%###..*##*::@@%%
;;  :--=******==*#%%*+*@@@@:@.-.....::----=*#%%%@%%%%*.:=.....-@+=....+@@@@@@%%%@%%%%%@-..#@@@@-%%%#=+-..:..:%@@@#:.-:......==-=.-.:-%@@@@+@+=#.-=@#*@@@@%*###+#####..*#####.-@@
;;  --::=*#*#*++*#%###%@@@@@@=.........::.-*+=+=+-++*@+-##=%:=@#...:%@@@@@@@@@@@@@%@%%@@@@@@@-..-+@#=+:.....:.+++..:*@%-......=..+%@@@@@-@*=%%@@#..-@@@@@**##*+#####..**######.:
;;  ----+**#*##**%%%%%%@@@@#:............:.......:.:+@@@:.:.#@#.....:-:=:.--.--:+@@@@@@%%#%@@@@-*%-=+=:...-::-**-:::::....++:.+%@@@@@@=##+=#**##=:..%-*@@*###**%####..#####***##
;;  -==:-+*#####%%%%%@@@@@+..........................-#@@@@@%-....-*+:-*%##%%@@@%#--@@@%%%@@@@%-#+===+:..:-..........:...=.=@@@@@@@@##@@+==+++*--..-:+*-#-###+*#####..####******
;;  --+-:=+#####%%@@@@@@@*.....................................:++=.-##%@@@#+:+%#%-*.@@@@%%%@@%-%%%+##+:.............--:-@@@@@@@@@@#@##*-+===+=...*@%@..+*###-######..######****
;;  *:-*+==*###%%@@@@@@:....:=+##%%#+++=-:..-..:.::........::*@+*..@@#.+@@*...+.:.=-=.@@@@-=-@%=%%%+++.........:......@@@@@@@@@@@*@@@@@**++==-=.*-@@@@@@@#..--#####*.:#*********
;;  ==-*#*+=#%%%@@@@@@=...:-+*#%@@@%@@%%##*+-+%@@##*%*=-:+#@#@@@-:@@%.:@...#-+.#-+.*..#@@@@@@@*=%%%++=..........*-.%@@@@@@@@@@@%%@@@@@*%#%##*:..:=-@@@@@@***+=-####*.:###*******
;;  =+-+#%#*%@@@@@@%-...-+##%@@@%%@%%@%%#%#*+@@@@@@@%@%@%@@@@@@#.##%@#.:#%*.#%=.*::--.%@@@@@@@*=%%#=+=....:+.-.:*@@@@@@@@@@@@@*%@@@@@@*%##*:=....@@@@@@@@****+++###+.-###*******
;;  ==--#%%*#@@@@@:...-+#%%%%@@@%%%%@***###-@@@%@@@@@@@@@@@@@@@+.=.=%%+.*:+.==@+=...*.@@@%@@@@*=##%-++....-:-=@@@@@@@@@@@@@@@@@@@@@%@%%%+..=#=...@@@@@@@%****=*####=.=##*#******
;;  ++*##*%%%@@@@:..:-+*#%#**#%@#%#*%*-+*+-+=-:@@@@@@@@@@@@@@%#+....-##.:@-=.#..:.=.=-@@@@@@@@*-###==+..+::@@@@@@@@@@@@@@@@@@@@@@@@%%*.-=%=*..-%-@@@@@@@#****-####*-.=**********
;;  ***####%@@@==...:+**#%%%%#*+#**+-=.::=..=-%@@@@@@@@@@@@#%-.#@@#-.=++.@=:.@:::=:.#:@@@@%*@@%:###.+-.-#@@@@@@@@@@@#@@@@@@@@%@@@@#-...#@=@.=....--@+%@@%****-##***:.=****+*****
;;  ##*##**%@@@#...:=+##%%#@@@@@@=:-....==*#+:.=@@@@#@@@@@%*::@@%+:..:::.+#..@=+%#-..+@@@@@@@@@-**..+:@@@@@@@@@@@@@%@@@@@@@@@%@%*.+...:@@.#%.::.+@@.@@@@%****-##***..+****+++++*
;;  *###%%%@@@@....::+*#%%@@@@@@@@#+.....+*+.::=@%@@#-@@#%*=:@@%=........-@@=.......:@@@@@@@@=-:.=.*@@@@@@@@@@@@@#%@@@@@@@@@@#:-...*.@@@@:.+-..**@@@%#@@#****-*#***..+*****+++*+
;;  #**%@@%@@@%....::=*#%%%@@@@@@@@@=...-++**#-:*.@**-=@-#=.%@%+.#%@@@@@.#-.........@@@@@%+@@=.::@@@@@@@@@@@@@@@%%@@@@@@@%%::=....*.-*.@@@......+#@@@@@@#****+*****..+******++++
;;  %##%@@@@@@*+....:-+*##%%@@@@@@@@@#..-=+**#-.-.+=*+.+:=.+@@*::#%@%@#--*........:@@@@@@::=.:+@@@#@@@@@@@@@@@@#@@@@@@@#+=.-.....=..-..@%-..+...#@##@@@@****+*****+..++++*++++++
;;  +##@@@@@@@@@.....:==++##%@%@@@@@@@@#..-=+*+*+-:+++-:==.@%%+....==.::+.....-#%#*%@+@@*.:.%@@@@%@@@@@@@@@@@@%@@@@@@*.*.......%*....-.+=....#..#@@%@@@@****=*****+..++++++++=+=
;;  %%%@@@@@@@@@%.....:-:-**#%@%@@@@@@@@@@@@+.=++=+=+==+*-=@#*:.@@+...+:-.:%=..::..:%:.=.-@@@@@@#@@@@@@@@@@@@@@@@@%..-.:-....*@:.....=....-...=.#@@@@@+*++**=*****+..++++++++=++
;;  %@@@@@@%@@@@@%......::-***##%%%@@@@@@@@@@@-.=+=-==..=.#**+..:.*-:=:*...=#+.......-.+@@@@@@@%@@@@@@@@@@@@@@@#:*...........-.#=...=#...--:...=#@@@@@@@=::*=*****=..++==-=++++=
;;  @@@@@@#:@#@@@@@.........-=++*##%%@@@@@@@@%#=.--..#@@%.@*+*.:.%-+-....+-.:%@@@=.+:#@@@@@@@@#@@@@%@@@%%@@@%+.=..-=......:.-..=.........-.*....@@@@@@@@+*+--+****=..==-====+===
;;  @@@@:-@:--..*@=...........-:=***%%%@@@%%##+...%@@@@@:-@+*..=.@....+:.:@@@@@@@@%.#@@@@@@@@#@@@@@@@@@%%@=:#..-........-..=.........+..........@@%@@@@%=***.-****=..=++=+++===.
;;  @@-:#::::-:--................-=***##%%#+=:.+@@@@#*#*.@#+=.-..-....-@@@@@@@@@@@@@-:%@@@@%*%%%%%@@@%%----:.................::....=...:-:---=:=%@@@@+%#+=++=*:***-..==+=====--.
;;  %.-.::::.......................:==*+**+..#@@@####*#==@++:.-..-.-@@@@%*%@@@@@#++%@#.*%@%%%%%%%%@%==.............+++@+@@@@.::-+@%@@@%@@@@@@@@@@%@@@@@*=+++=**++*-..-==-::.....
;;  @...:::....:....................:--==+..@@%#*+*****.%*=#....:@@@@@@%%%%@@%.......@@.+%%%%%@%#+.................-%#@@@@@-%@@:@@*#@@@@@@@%%@+@@@@@@@@*++++-+***+:........:*@@@
;;  @...................................:..%#*+++*+***-.@++-..=@@@%@@@%@%@@@*...@@@..#@=:#%%%%=.*...............:=##%%@@-*%@@@*#@@@%+@@@@@@=%@#@@@@##@@+=++*-++*++:....-%@@@%@%%
;;  @-........:...:-=++++=-:..............=%*-=+**:.=*.*#=+..%@@%%%%%@%@@@@@+...:=-.:@@@.+#:....+..............:*+%#*#@@@@@@@@@@@@@@@@@@@@@@@#.@@@@@*%@=++++:++++=.-@@@@@@%%#%%%
;;  -*......:....:==+#%@@%%#+-............:@+==-=*:+++.@==-.:@%%%%%%%%%@%%%%@=--:.=@@#@@+....@@@@*:............-+**##%%%%%@%%@@@@@@@@@@@@@@@@*..#%@@@%+-=+++:+++=-.#@@%%%%%####%
;;  :...........:-*#%%@@@@@@@%*=:..........:%*..=+*++:=#-+..#%%%%%%%%%%*%@%%%@@@@@@@@%%@-.:@@@@@@@@@@@@@@@@@@#**+-:.............................................:+@@@@@#%%#*###%
;;  @+......-.:.=+#%%@@@@@@@@@@#+-..........:@**+++++.%=-*..@%%%%%%#*%%@%#%%%%%%@@%%%%*...*@@@@@@@@@@@@@@@@@@@@@@@@@@.........................-..........@@@@@@@@@@@%@%@%%%##%%@
;;  @@%:......:-*##@@@#@@@@@@@@%#*:.......#+..#*+++=..#-+:.=@%%####%%%%%%#%%%@@%%%%+.....*@@@@@@@@@@@@@@@@@@@@@@=:........::--------:.......................=@@@@@@@@@@@@@@@@@@@
;;  @@@@@*...:=+*#%%*#@@@@@@@@%%#=-.....+@@#-.:*=::=.=+-+..@%*#%%%%%#%%%%#*=%%@%+......:%@@@@@@@@@@@@@@@%%#*+==...:-*%@@@@@@@@@@@@@%##**#**#*+==:....................-=+**##%@@@
;;  @@@@@@%...:+*####%%%@%%%%%%##=-....@@###*-..#*...@==-.:@#%%%%%#%%%%%%%%@%:......=@@@@@@-...........................:-=**%%@@%=+*%@@@%##*##+=-:................-=**##***#%@@@
;;  %%@@@+....-=+***###%%%%###*++-...%@%%#*#*+:..+#*.#=+..=%%##%%#*%#=%%%+......-%@@@@@@@:...........:-+%%@@@@@@%%%%%@%@%%@@@++=::=***######%%@@%%%%@@@@@@+=@@@@@@@@@@%#*+++====
;;  @@@@*-....--=****#*#####**+=-..+@####**:......##:..:..*#%%##*%%%-*-.........@@@@@@@@@-............-=##%*@@@%@@@@@%@@@@@@-*#@@@@@@@@@%@@@@@@@@+%@@%%###+==++++*++++++++++=--:
;;  @%@@@......-====++******+=--.:@##*#**.........-@+:....%%**#+#%+............=@@@@@@@@@-...........:-:+#########%%%%%%%%%%:.#%%##*%@%%-*%#%@#=+=-******+*-+++++=+:=.=::=:=.::.
;;  %%@@@-......::-:-=+=++==-:..%%###*=............:#::...%*+-=......:........=@@@@@@#@@@-...........-=-+=###+###%%%%%%%%%%%#=#%%%%%%%%%@%@%%%#+++-***-=:+:*.+.-:.-.=.-::=.=.::.
;;  %%@@@@.........::---=:=::.:@#**+................*=-...=*:................+@@@@@@@#@@@-........:-=+++==+##*##%#*-....:--=+*##%%%%@@@@%%%%%%#+++-**+:-.+.+:=.-:.-.-.=::-.=.::.
;;  %%@@@@*..........::...:..:@##=.................+@::..............=.....+@@@@@@@@@@@@@-........:-=====+-*+####*#*-+....................*@@@#+++-+*+::.+:+.=.-..-.-.-::=.=.:..
;;  **%#@=....................%@*-.............:#@@#+=....*@@@%..................+@@@@@@@=.........:--=++*+--****##%@@@##*++=+%#@:#@@@#*+=*%%%#+++-*+=-:.+.+.+.-..-.-.=.:-.-.:..
;;  ***#@@*....................%@#=.........=@@@#+...*@@@@@@@@@%.....................%@@@#.......::-====++*-=-*+**####**####%%##%+=%%%%##@%%###+=+-+++-..=.+.+.-..-.-.-:::.-.:..
;;  ...-%#......................#@#+.....=@@@*-..+@@@@%=-....:=-.:++.:-:...............%@%....:::::-+++=+++=++************#######*-%%%%%%%%##+#===:+++:..-.+.=.:..:.-.-..:.-.:..
;;  ..:+..:.-+#####**==..........*@@@#*@@#+:..#@+:...............:+.-#*.%@==@@-*@%.*#:.:-:...:--====+++++++++++++++=***+.:-****###.+=.+=.*%##*#===:=++:..-.=.-.:..:.::#%.:.-::--
;;  .+..=#++@%@@@@-%@@%%*::.......+####-..*=..............:++:#*#@*%@@-%%=.........+-.%@#.%@@-.@@@=-#+.:..:==++***++*+*=+:.*+=*=+*=.=#..*.*##**==-.===-..:.=.=:=-:=@@@-.:-------
;;  ...+#:=#@@@@@-@@@@@%#+............-:....:.......=+-+=-@#*%%=*#*.:-:.:.+%=#@@-@@#:%%+.:.....:=..*%:.@@%.+@@@--**=.++-:.:-=+++***=.#:-*.####*=--.===-:--==-.-*%@@@%%..::::....
;;  ..:-+.=%@@@#=+@@%@%#*::...................+*.*##:%@@**+=::...:.+#-+@*@@#*#+::....--:##%:.-@@*-%#=.......:-.=@@#:.=@@@@@@@-:*+*++*-::=#####*=--.=--======:.-*@@%%*......:-==*
;;  ..:::.:*###*.+%#%##=..................*@@@@@%=:...::..:-:=*=*@@@@#=.....-.:%%*@@#*@@*.........-+.+@@@##+==**-....+%@@#:.::.+++++**+.+*****+---.---------:........=###%#%%%%%
;;  -.......--:........=@@@#+%%@@@@%*+++=::=----::=#@@@@@@@@%+:....::-::-*%@@@%:::.......-=@@@-#@@+=@@%.-*#=-:.::#@@@#.::----=.........=:-+****--:.......:==#%#%%=..=+*#%#**####
;;  @@@@@@%%#@@@@@@@@@@%@@@@@@@@=#@@@#%@@@=-.....:-===---:===+%@@@@@@%#*=:...:..-+*..+%@=*#*+=......:.+*=....*@@@#=:::::-..-#%*#@@@@%*:.:=:.....-+*#@@@@@%@%@@@#-...:-=#%%#*=-==
;;  #%%*#%%%@@@@@@@@%+%#-#@@@@@@@@+%@@@@%%%%%@@@@@@*+-....::-----::::-*%@@@@@@@%#+=....-.:-=-.:+%##=:...:+@@@%=:-::::::.=@@@.-=@@@@@@@@@%.:%@@@@@@@@@@+:=#@+#@@@@@@@@@@@%%#%###%
;;  #%%%%%%%@@@@@@:++:.+%@@@@#-*:%@@@=.###*-.-@@%+#@++@@@@@@@#-.....:==--===--..=#@@@@@@@@@%+:.......*@@@%-::::.::::..+@@@@@@@@@@@@@#**+++=.*@@@@@@@@@@@*@@@*@@@@#*##+***###****
;;  ####%%%@%%@@@@###%@@@@@@@:...-*@@#..:-+#%@@@@@@@@@%@@@@@@@@@@@@@#*##*+:...:.-===-:--::-=*@@@@@@@@%=:::::::.....-.+@@%@@@@@@@@#*+-........--*%@@@@@@@@@@@@%%%%@@%%%%%#%#***+*
;;  *#####**++====+*#####*##*####%%@@@@@@%#*@@@#=@@@@.+%*###@@@=-.-+=-#@@@@@@@@@**=::.....:--=-:::.:--::....-*%@@@@@..=+*%%%%##*+:.:...............:-=++*#%%%@@@@%%**+++====-=+=
;;  -::--+****####*+=-=*######%##%%%%%#%###%@@%*%@@@%:.-=:...@@@@@@@@@@@@-%@@@+.:..%@@@@@@@%*=.......:*@@@@@@@@@@@@@#..::.......:.........+%@@@@@@@@#=:....:-=-::-+++***+++=====
;;  ::-+++-.:-=+***++**+++++**###******++****#####%##+..-+#%@@@%%%@%@@@@%%%%@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@*-........:=%@@@@@@@%%@@%%#%%%#***####*#*=:...:==:.......
;;  .....::---=-------=====++**+=:.-=++*****+=-:-=**#**###*+=-+**++###*++****########%%%####*******#*##%%#*****+***#**#%%@@@@@@%%#%%%%#***##**++*+++++=+++++====+======-:.......
;;  .............::::-:--:::..:-=::.:----==+*+=-:-==-====+++++==+++++=+====+++=+++***++*++++++++***+++=+=+-=========--=====+======-====---======--:.:-::::::-:....:::.:::.......
;;  ...................:..::..:--......:.:......-:-------:--..:-==-----:........:-:--:-----::-..::....:::::::::-...::.::.:......:::--.....::...-:...............................

(def harvest-on-boot? (get (config/settings) :harvest-on-boot? true))

(defn get-last-modified-time
  [f-path]
  (let [path          (Paths/get f-path (into-array String []))
        instant       (.toInstant (Files/getLastModifiedTime path (into-array LinkOption [LinkOption/NOFOLLOW_LINKS])))
        zonedDateTime (.atZone instant (ZoneId/systemDefault))
        formatter     (DateTimeFormatter/ofPattern "yyyy-MM-dd HH:mm:ss")]
    (.format formatter zonedDateTime)))

(def invalid-to-valid-keywords {":>" ":reagent-gt"})
(def valid-to-invalid-keywords (zipmap (vals invalid-to-valid-keywords) (keys invalid-to-valid-keywords)))

(defn convert-to-valid-edn [s]
  (reduce (fn [s [invalid valid]]
            (clojure.string/replace s invalid valid)) s invalid-to-valid-keywords))

(defn convert-back-to-original [m]
  (clojure.walk/postwalk
   (fn [x] (if (and (keyword? x) (valid-to-invalid-keywords x))
             (valid-to-invalid-keywords x) x)) m))

(defn read-screen [f-path]
  (let [screen-str    (slurp f-path)
        valid-edn-str (convert-to-valid-edn screen-str)
        screen-data   (try (edn/read-string valid-edn-str) (catch Exception e (ut/pp [:read-screen-error!!!!! f-path e]) {}))]
    (convert-back-to-original screen-data)))

(defn update-screen-meta [f-path]
  ;(qp/slot-queue :update-screen-meta f-path
  (ppy/execute-in-thread-pools :update-screen-meta
                               (fn []
                                 (try
                                   (let [;file-path "./screens/"
                                         screen-data      (read-screen f-path)
                                         screen-name      (get screen-data :screen-name "unnamed-screen!")
                                         resolved-queries (get screen-data :resolved-queries)
                                         materialized-theme (get screen-data :materialized-theme)
                                         screen-data      (if (ut/ne? resolved-queries)
                                                            (assoc screen-data
                                                                   :panels (into {}
                                                                                 (for [[k v] (get screen-data :panels)]
                                                                                   (try
                                                                                     {k (assoc v :queries (select-keys resolved-queries (keys (get v :queries))))}
                                                                                     (catch Throwable e (ut/pp [:error-reading-screen-panels f-path e k v]) {}))))) ;; issue with pomp-girl2? TODO
                                                            screen-data)
                                         theme-map        (get-in screen-data [:click-param :theme])
                                         has-theme?       (and (not (nil? theme-map)) (ut/ne? theme-map))
                                         has-materialized-theme?       (and (not (nil? materialized-theme)) (ut/ne? materialized-theme))
                                         theme-name       (if (and has-theme? (not (nil? (get theme-map :theme-name))))
                                                            (str ", " (get theme-map :theme-name) " ")
                                                            "")
                                         blocks           (remove nil? (conj (keys (get-in screen-data [:panels]))
                                                                             (when has-theme? :*theme*)
                                                                             (when has-materialized-theme? :*materialized-theme*)))
                                         boards           (distinct (for [[_ v] (get-in screen-data [:panels])] (get v :tab)))
                                         blocks           (into blocks boards) ; (map #(str "board/" %) boards))
                                         params           (get-in screen-data [:click-param])
                                         board-map        (into {}
                                                                (for [b boards]
                                                                  {b (into {} (for [[k v] (get-in screen-data [:panels]) :when (= (get v :tab) b)] {k v}))}))
                                         queries          (into {} (for [b blocks] (get-in screen-data [:panels b :queries])))]
                                     (doseq [b blocks] ;; collect keywords from all views for autocomplete - might get
                                       (doseq [[_ vv] (get-in screen-data [:panels b :views])]
                                         (reset! wss/autocomplete-view-atom (vec (filter #(and (not (cstr/includes? (str %) "/")) (keyword? %))
                                                                                         (distinct (into (vec (distinct (ut/deep-flatten vv)))
                                                                                                         @wss/autocomplete-view-atom)))))))
                                     (swap! db/screens-atom assoc screen-name screen-data) ;; update master screen atom for
                                     (sql-exec system-db (to-sql {:delete-from [:screens] :where [:= :file_path f-path]}))
                                     (sql-exec system-db (to-sql {:delete-from [:blocks] :where [:= :file_path f-path]}))
                                     (sql-exec system-db
                                               (to-sql {:insert-into [:screens]
                                                        :columns     [:file_path :screen_name :blocks :queries]
                                                        :values      [[f-path screen-name (count blocks) (count (keys queries))]]}))
                                     (sql-exec system-db
                                               (to-sql
                                                {:insert-into [:blocks]
                                                 :columns     [:file_path :screen_name :block_key :block_name :views
                                                               :queries :view_names :query_names :block_data
                                                               :tab_name]
                                                 :values      (for [b blocks]
                                                                (let [theme?     (true? (= b :*theme*))
                                                                      materialized-theme? (true? (= b :*materialized-theme*))
                                                                      board?     (some #(= b %) boards) ;(cstr/starts-with? (str b)
                                                                      queries    (get-in screen-data [:panels b :queries])
                                                                      views      (if theme? theme-map (get-in screen-data [:panels b :views]))
                                                                      block-name (str (cond theme? (str "(meta: this screen's theme" theme-name ")")
                                                                                            board? (str "board: " b)
                                                                                            :else  (get-in screen-data [:panels b :name])))]
                                                                  [f-path screen-name (str b) block-name (count (keys views)) (count (keys queries))
                                                                   (try (str (cstr/join " " (keys views))) (catch Exception _ ""))
                                                                   (try (str (cstr/join " " (keys queries))) (catch Exception _ ""))
                                                                   (cond theme? (str theme-map)
                                                                         materialized-theme? (str (get materialized-theme :theme))
                                                                         board? (pr-str {:panels (get board-map b) :click-param {:param (get params :param)}})
                                                                         :else  (str (get-in screen-data [:panels b])))
                                                                   (str (get-in screen-data [:panels b :tab]))]))})))
                                   (catch Throwable e
      ;(ut/pp [:update-screen-meta-error! f-path e])
                                     (println "Error in update-screen-meta:" (.getMessage e))
                                     (.printStackTrace e))))))

(defn update-all-screen-meta []
  (sql-exec system-db (to-sql {:delete-from [:screens]})) ;; just in case
  (let [prefix "./screens/"
        files  (ut/get-file-vectors-simple prefix ".edn")]
    (doseq [f files] (update-screen-meta (str prefix f)))))

(defn update-flow-meta [f-path]
  ;(qp/slot-queue :update-flow-meta f-path
  (ppy/execute-in-thread-pools :update-flow-meta
                               (fn []
                                 (try (let [screen-str (slurp (str f-path))
                                            flow-data  (edn/read-string screen-str)
                                            flow-id    (get flow-data :flow-id "unnamed-flow!")
                                            insert-sql {:insert-into [:flows]
                                                        :values      [{:flow_id       (str flow-id)
                                                                       :components    (count (get flow-data :components))
                                                                       :last_modified (get-last-modified-time f-path)
                                                                       :connections   (count (get flow-data :connections))
                                                                       :file_path     (str f-path)
                                                                       :body          (pr-str flow-data)}]}]
                                        (sql-exec systemh2-db (to-sql {:delete-from [:flows] :where [:= :file_path f-path]}))
                                        (sql-exec systemh2-db (to-sql insert-sql)))
                                      (catch Exception e (ut/pp [:read-flow-error f-path e]))))))

(defn update-all-flow-meta []
  (sql-exec systemh2-db (to-sql {:truncate :flows})) ;; just in case
  (let [prefix "./flows/"
        files  (ut/get-file-vectors-simple prefix ".edn")]
    (doseq [f files]
      (let [f-path (str prefix f)]
        (wss/create-flowblock f-path)
        (update-flow-meta f-path)))))

(defn refresh-flow-fn [flow-functions-map]
  (doseq [{:keys [category name description file-path inputs icon types]} (ut/flatten-map flow-functions-map)]
    ;(wss/enqueue-task3 ;; since out base sqlite db hate concurrency, with a "real" db, this can
    (qp/serial-slot-queue :general-serial :general
    ;(ppy/execute-in-thread-pools :general-serial
                          (fn []
                            (let [connection_id "system-db"]
                              (sql-exec system-db (to-sql {:delete-from [:flow_functions] :where [:and [:= :name name] [:= :category category]]}))
                              (sql-exec system-db
                                        (to-sql {:insert-into [:flow_functions]
                                                 :columns     [:connection_id :run_id :category :name :full_map :description :inputs :icon
                                                               :input_types :output_types :file_path]
                                                 :values      [[connection_id 0 ;;this-run-id
                                                                (str category) (str name) (str (get-in flow-functions-map [category name])) ;; full
                                                                (str description) (str inputs) (str icon) (str (vec (vals (dissoc types :out))))
                                                                (str (get types :out)) (str file-path)]]})))))))

(defn watch-flows-folder []
  (let [file-path "./flows/"]
    (beholder/watch #(when (cstr/ends-with? (str (get % :path)) ".edn")
                       (let [f-path (str (get % :path))
                             f-op   (get % :type)
                             ffn    (wss/create-flowblock f-path)]
                         (refresh-flow-fn {:sub-flow ffn}) ;; change the sql for the sub-flow block
                         (ut/pp [:flow-change! f-op f-path])
                         (update-flow-meta f-path)))
                    file-path)))

(defn get-ai-workers []
  (try
    (let [workers-dir (io/file "./ai-workers")
          worker-folders (filter #(.isDirectory %) (.listFiles workers-dir))
          workers-map (into {}
                            (for [folder worker-folders
                                  :let [folder-name (.getName folder)
                                        config-file (io/file folder "config.edn")
                                        system-prompt-file (io/file folder "system-prompt.txt")
                                        personality-file (io/file folder "personality.txt")]]
                              (when (.exists config-file)
                                (try
                                  (let [config (edn/read-string (slurp config-file))
                                        system-prompt (if (.exists system-prompt-file)
                                                        (slurp system-prompt-file)
                                                        "")
                                        personality (if (.exists personality-file)
                                                      (slurp personality-file)
                                                      "")]
                                    {folder-name (assoc config
                                                        :system [{:type "text"
                                                                  :text personality}
                                                                 {:text system-prompt
                                                                  :type "text"
                                                                  :cache_control {:type "ephemeral"}}])})
                                  (catch Exception e
                                    (ut/pp [:error-reading-ai-worker-files folder-name (.getMessage e)])
                                    nil)))))
          trunc-workers-map (ut/deep-remove-keys workers-map [:system :tools])]
      (try
        (reset! db/model-costs (edn/read-string (slurp "./ai-workers/model-costs.edn")))
        (catch Exception e (ut/pp [:error-reading-model-costs.edn! (str e)])))

      (when (seq workers-map)
        (swap! db/ai-worker-atom assoc-in [:config :server] workers-map)
        (swap! db/ai-worker-atom assoc-in [:config :client] trunc-workers-map))
      true) ; true if successful
    (catch Exception e
      (ut/pp [:error-in-get-ai-workers (.getMessage e)])
      false)))

;; (ut/pp  (get-ai-workers))

(defn watch-ai-workers-folder []
  (let [file-path "./ai-workers/"]
    (beholder/watch
     (fn [event]
       (when (and (= (:type event) :modify)
                  (or (cstr/ends-with? (str (:path event)) ".edn")
                      (cstr/ends-with? (str (:path event)) ".txt")
                      (cstr/ends-with? (str (:path event)) ".clj")))
         (let [f-path (str (:path event))
               f-op (:type event)]
           (ut/pp [:ai-worker-change! f-op f-path])
           (when (get-ai-workers)
             (ut/pp [:ai-workers-updated-successfully])))))
     file-path)))

(defn merge-shape-files []
  (let [file-path "./defs/shapes/"
        files (->> (ut/get-file-vectors-simple file-path ".edn")
                   (sort-by str))
        merged-shapes (reduce
                       (fn [acc file]
                         (try
                           (let [full-path (str file-path file)
                                 shape-data (edn/read-string (slurp full-path))
                                 shape-set (-> file cstr/lower-case (cstr/replace ".edn" ""))
                                 shape-data (into {} (for [[k v] shape-data] {k (assoc v :shape-set shape-set)}))
                                 _ (ut/pp ["🎲🃏" :shapes full-path (count (keys shape-data))])]
                             (merge acc shape-data))
                           (catch Exception e
                             (ut/pp [:error-reading-shape-file file (.getMessage e)])
                             acc)))
                       {}  ; to acc into
                       files)]
    (reset! db/shapes-map merged-shapes)))

;; (ut/pp (keys (merge-shape-files)))

(defn watch-shapes-folder []
  (let [file-path "./defs/shapes/"]
    (beholder/watch
     (fn [event]
       (when (and (= (:type event) :modify)
                  (cstr/ends-with? (str (:path event)) ".edn"))
         (let [f-path (str (:path event))
               f-op (:type event)]
           (ut/pp ["🎲🃏" :shape-files-change! :merging f-op f-path])
           (merge-shape-files)
           (db/ddb-clear! "honeyhash-map")
           (db/ddb-clear! "shapes-map"))))
     file-path)))

(defn watch-screens-folder []
  (let [file-path "./screens/"]
    (beholder/watch #(when (cstr/ends-with? (str (get % :path)) ".edn")
                       (let [f-path (str (get % :path))
                             f-op   (get % :type)]
                         (ut/pp [:screen-change! f-op f-path])
                         (update-screen-meta f-path)))
                    file-path)))

(defn shape-rotation-run [f-path & [pre-filter-fn]]
  (let [connection-id (rota/get-connection-id f-path)
        _ (ut/pp ["🥩" :running-full-shape-rotation-job connection-id (str f-path)])
        _ (swap! db/shape-rotation-status assoc-in [connection-id :all :started] (System/currentTimeMillis))
        conn-map (if (map? f-path) f-path (get @db/conn-map connection-id))
        _ (sql-exec systemh2-db
                  (to-sql {:delete-from [:fields] ;; clear last - in case a table was deleted (we clear each table before we add, but dont address orphans)
                           :where [:= :connection_id (str connection-id)]}))
        pre-filter-fn (when (= connection-id "systemh2-db") '(fn [m] (and (not= (get m :db-schema) "INFORMATION_SCHEMA")
                                                                         (not= (get m :db-schema) "PG_CATALOG"))))
        res (rota/get-field-maps {:f-path f-path
                                  :shape-test-defs cruiser/default-sniff-tests
                                  :shape-attributes-defs cruiser/default-field-attributes
                                  ;;:filter-fn nil ;(fn [m] (true? (get m :very-low-cardinality?)))
                                  :pre-filter-fn (or pre-filter-fn (get conn-map :pre-filter-fn))})
        combo-viz (rota/get-viz-shape-blocks res @db/shapes-map)
        shapes-by (group-by (fn [m] [(get-in m [:shape-rotator :honey-hash])
                                     (keyword (get-in m [:shape-rotator :context 3]))]) combo-viz)
        fields-by (group-by (fn [m] [(get m :honey-hash)
                                     (keyword (get m :table-name))]) res)
        ;; _ (ut/pp [:kk (keys shapes-by) (keys fields-by)])
        _ (doseq [kk (keys fields-by)
                  :let [[honey-hash table-name-kw] kk
                        shapes (get shapes-by kk)
                        fields (get fields-by kk)
                        res {:shapes shapes :fields fields}
                        hash-key (hash [:fresh-table! connection-id table-name-kw])
                        modded-shapes (assoc res :shapes (mapv #(assoc-in % [:shape-rotator :source-panel] [:fresh-table! connection-id table-name-kw])
                                                               (get res :shapes)))]]
            (d/transact-kv db/ddb
                           [[:put "honeyhash-map" honey-hash res]
                            [:put "shapes-map" hash-key modded-shapes]]))
        ;; group by honeyhash - do artificial inserts into datalevin
        ;; "[\"none!\" nil :Crime_Data_from_2020_to_Present]"
        ;;_ (ut/pp ["🥩" :shape-rotator-db {:fields (take 2 res) :shapes (take 2 combo-viz)}])
        _ (ut/pp ["🥩" :shape-rotator-fields [:fields (count res) :viz (count combo-viz)]])
        _ (swap! db/shape-rotation-status assoc-in [connection-id :all :ended] (System/currentTimeMillis))
        _ (swap! db/shape-rotation-status assoc-in [connection-id :all :viz-combos] (count combo-viz))]))

;; (time (shape-rotation-run "connections/bigfoot-ufos.edn"))
;; 5000 1652 1190 1208 1359 1246

(defn db-sniff-solver-default [f-path conn-name]
  {:signal :signal/daily-at-9am
   :type   :clojure
   :cache? false
   :data   (walk/postwalk-replace
            {:conn-name conn-name
             :f-path f-path}
            '(do
               (ns rvbbit-backend.core
                 (:require [rvbbit-backend.websockets :as wss]
                           [rvbbit-backend.pool-party :as ppy]
                           [rvbbit-backend.sql :as sql]))
               (let [conn (get (deref db/conn-map) :conn-name)]
                 (shape-rotation-run :f-path))))})

;; (ut/pp (db-sniff-solver-default "./connections/postgres.edn" "postgres"))
;; (ut/pp (keys @db/conn-map))
;; (ut/pp @db/shape-rotation-status)
;; (shape-rotation-run systemh2-db)
;; (shape-rotation-run cache-db)

;; (wss/fig-render ":db-shape-rotator" :bright-cyan) ;; :solvers :nrepl-calls :websockets
;; (ut/pp
;;  (into {} (for [[k v] (deref rvbbit-backend.db/shape-rotation-status)
;;        :let [vv (get v :all)]]
;;    {k (-> vv
;;           (assoc :time-taken (rvbbit-backend.util/format-duration-seconds (try (/ (- (get vv :ended) (get vv :started)) 1000) (catch Exception _ -1))))
;;           (assoc :started (try (rvbbit-backend.util/ms-to-iso8601 (get vv :started)) (catch Exception _ -1)))
;;           (assoc :ended (try (rvbbit-backend.util/ms-to-iso8601 (get vv :ended)) (catch Exception _ -1))))})))

;; (rvbbit-backend.util/ms-to-iso8601 (System/currentTimeMillis))

(defn update-all-conn-meta []
  (let [prefix "connections/"
        files  (ut/get-file-vectors-simple prefix ".edn")
        ;; current-conn-ids (vec (for [f files ;; remove "missing" DBs - TODO more robust archive and reappear
        ;;                             :let [f-path    (str prefix f)
        ;;                                   conn-name (str (cstr/replace (cstr/lower-case (last (cstr/split f-path #"/"))) ".edn" ""))]]
        ;;                         conn-name))
        ]
    (doseq [f    files
            :let [f-path    (str prefix f)
                  conn-name (str (cstr/replace (cstr/lower-case (last (cstr/split f-path #"/"))) ".edn" ""))
                  solver-name (keyword (str "refresh-" conn-name))
                  solver-edn-fp "defs/solvers.edn"
                  solver-exist? (get @wss/solvers-atom solver-name)
                  _ (when (not solver-exist?)
                      (ut/pp [:refresh-metadata-schedule-being-created-for conn-name :as solver-name])
                      (swap! wss/solvers-atom assoc solver-name (db-sniff-solver-default f-path conn-name)) ;; TODO why does this fail now?
                      (fpop/freeze-atom solver-edn-fp)
                      (ut/zprint-file solver-edn-fp {:style [:justified-original] :parse-string? true
                                                     :comment {:count? nil :wrap? nil} :width 120
                                                     :map {:comma? false :sort? false}})
                      (wss/reload-solver-subs))
                  conn      (edn/read-string (slurp f-path))
                  poolable? (try (and (map? conn) (find conn :jdbc-url)) (catch Exception _ false))
                  conn      (if poolable?
                              (try {:pre-filter-fn (get conn :pre-filter-fn)
                                    :datasource @(pool-create conn (str conn-name))}
                                   (catch Exception e (do (ut/pp [:connection-error conn-name e]) {:datasource nil})))
                              conn)]]
      (try ;; connection errors, etc
        (do (when poolable? (swap! db/conn-map assoc conn-name conn))
            (when harvest-on-boot?
              ;(qp/slot-queue :schema-sniff f
              (ppy/execute-in-thread-pools :conn-schema-sniff-serial
                                           (fn []
                                            ;;  (time
                                            ;;   (cruiser/lets-give-it-a-whirl-no-viz f-path
                                            ;;                                        conn
                                            ;;                                        system-db
                                            ;;                                        cruiser/default-sniff-tests
                                            ;;                                        cruiser/default-field-attributes
                                            ;;                                        cruiser/default-derived-fields
                                            ;;                                        cruiser/default-viz-shapes))
                                            ;;  (time (ut/pp [:shape-rotator-fields
                                            ;;                (let [res (rota/get-field-maps {:f-path f-path
                                            ;;                                                :shape-test-defs cruiser/default-sniff-tests
                                            ;;                                                :shape-attributes-defs cruiser/default-field-attributes
                                            ;;                                                :filter-fn nil ;(fn [m] (true? (get m :very-low-cardinality?)))
                                            ;;                                                :pre-filter-fn nil})
                                            ;;                      combo-viz (rota/get-viz-shape-blocks res cruiser/default-viz-shapes)]
                                            ;;                  [:boot :fields (count res) :viz (count combo-viz)])]))
                                             (ppy/execute-in-thread-pools :shape-rotation-jobs-serial (fn [] (shape-rotation-run f-path)))
                                             ))))
        (catch Exception e (do (swap! db/conn-map dissoc conn-name) (ut/pp [:sniff-error conn-name e])))))
    ;; (cruiser/clean-up-some-db-metadata current-conn-ids)
    ))

;; (update-all-conn-meta)

(defn watch-connections-folder []
  (let [file-path "./connections/"]
    (beholder/watch
     #(cond (cstr/ends-with? (str (get % :path)) ".edn") (let [f-path    (str (get % :path))
                                                               f-op      (get % :type)
                                                               conn      (edn/read-string (slurp f-path))
                                                               conn-name (str (cstr/replace (cstr/lower-case
                                                                                             (last (cstr/split f-path #"/")))
                                                                                            ".edn"
                                                                                            ""))
                                                               conn      (if (try (and (map? conn) (find conn :jdbc-url))
                                                                                  (catch Exception _ false))
                                                                           {:datasource @(pool-create conn
                                                                                                      (str conn-name "-pool"))}
                                                                           conn)]
                                                           (ut/pp [:connection-change! f-op f-path])
                                                           (swap! db/conn-map assoc conn-name conn)
                                                          ;;  (cruiser/lets-give-it-a-whirl-no-viz f-path
                                                          ;;                                       conn ;f-data
                                                          ;;                                       system-db
                                                          ;;                                       cruiser/default-sniff-tests
                                                          ;;                                       cruiser/default-field-attributes
                                                          ;;                                       cruiser/default-derived-fields
                                                          ;;                                       cruiser/default-viz-shapes)
                                                           (ppy/execute-in-thread-pools :shape-rotation-jobs-serial (fn [] (shape-rotation-run conn)))
                                                           )
            (cstr/ends-with? (str (get % :path)) ".csv") (let [f-path (str (get % :path))
                                                               f-op   (get % :type)
                                                               clients (vec (keys @wss/client-queues))]
                                                           (let [destinations clients]
                                                             (doseq [d destinations]
                                                               (wss/alert! d
                                                                           [:v-box :justify :center :style {:opacity 0.9} :children
                                                                            [[:box :style {:color :theme/editor-outer-rim-color :font-weight 700}
                                                                              :child (str "Note: CSV file being imported to cache-db")]
                                                                             [:box :child (str (get % :path))]]]
                                                                           14 2
                                                                           11)))
                                                           (ut/pp [:csv-file-change! f-op f-path])
                                                           (wss/process-csv {} f-path)
                                                           (let [destinations clients]
                                                             (doseq [d destinations]
                                                               (wss/alert! d
                                                                           [:v-box :justify :center :style {:opacity 0.9} :children
                                                                            [[:box :style {:color :theme/editor-outer-rim-color :font-weight 700}
                                                                              :child (str "Note: CSV file import finished")]
                                                                             [:box :child (str (get % :path))]]]
                                                                           14 2
                                                                           11)))))
     file-path)))


(defn clear-all-leaf-caches [& [message]]
  (ut/pp ["🍂" message "🌰 🍂 🐿️"])
  (reset! leaves/visible-views-cache {})
  (reset! leaves/keypaths-cache {})
  (reset! leaves/context-map-cache {})
  (reset! leaves/leaf-fn-cache {})
  (reset! leaves/leaf-eval-runstream-parallel-cache {})
  (reset! leaves/leaf-fns-cache {})
  (reset! leaves/leaf-fns-cache2 {})
  (reset! leaves/leaf-ql-cache {})
  (reset! leaves/fn-context-map-cache {})
  (reset! wss/shape-rotation-meta-cache {})
  (reset! wss/leaf-eval-cache {})
  (doseq [client-name (mapv first (leaves/active-clients))]
    (wss/notify-leaf-defs-updated client-name message)
    ;;(ut/pp [:client client-name :leaf-changes :re-evaluating-leaves!])
    ;; (let [rt :p ;(rand-nth [:p :s])
    ;;       f-run (get (ut/timed-exec ((if (= rt :p) leaf-eval-runstream-parallel leaf-eval-runstream) :fields client-name)) :elapsed-ms)
    ;;       v-run (get (ut/timed-exec ((if (= rt :p) leaf-eval-runstream-parallel leaf-eval-runstream) :views client-name)) :elapsed-ms)]
    ;;   (swap! db/leaf-bench assoc-in [client-name (if (= rt :p) :parallel :serial) :fields] (conj (get-in @db/leaf-bench [client-name (if (= rt :p) :parallel :serial) :fields] []) f-run))
    ;;   (swap! db/leaf-bench assoc-in [client-name (if (= rt :p) :parallel :serial)  :views] (conj (get-in @db/leaf-bench [client-name (if (= rt :p) :parallel :serial)  :views] []) v-run)))
  ;;  (leaves/leaf-eval-runstream-parallel :fields client-name)
  ;;  (leaves/leaf-eval-runstream-parallel :views client-name)
    ;; (leaves/leaf-eval-runstream :fields client-name)
    ;; (leaves/leaf-eval-runstream :views client-name)
    ;(leaf-eval-runstream-parallel-cached client-name)
    (ut/pp ["🍂🍂🍂" :leaves-re-eval :clear-caches client-name])))

;; (clear-all-leaf-caches)

(defn watch-config-files []
  (let [file-path "./defs/"]
    (beholder/watch #(do
                       (when (cstr/ends-with? (str (get % :path)) "leaves.edn")
                         (ut/pp ["🍂" :leaves.edn-changed! "🌰 🍂 🐿️"])
                         (clear-all-leaf-caches "leaf defs changed, reloading..."))

                       (when (or (cstr/ends-with? (str (get % :path)) "config.edn")
                                 (cstr/ends-with? (str (get % :path)) "clover-templates.edn"))

                         (let [destinations (vec (keys @wss/client-queues))]
                           (doseq [d destinations]
                             (wss/alert! d
                                         [:v-box :justify :center :style {:opacity 0.7} :children
                                          [[:box :style {:color :theme/editor-outer-rim-color :font-weight 700}
                                            :child (str "Note: Server config has been updated & received")]
                                           [:box :child (str (get % :path))]]]
                                         13 2
                                         5)
                             (wss/kick d [:settings] (wss/package-settings-for-client :rvbbit) 1 :none (str "file updated " (get % :path)))))))

                    file-path)))

(defn process-snap-file [file-path]
  ;(ut/pp [:processing-file file-path])
  (let [file-name (-> file-path io/file .getName)
        [_ base-name timestamp extension] (re-find #"^(.+)-(\d+)\.(\w+)$" file-name)]
    (when (and base-name timestamp)
      (let [full-name (str base-name "-" timestamp)]
        ;(ut/pp [:processed-file base-name full-name])
        {(keyword base-name) #{full-name}}))))

(defn update-macro-undo-map [file-path]
  ;(ut/pp [:updating-atom-for file-path])
  (when-let [processed-file (process-snap-file file-path)]
    (swap! db/params-atom
           (fn [current-state]
             (let [new-state (update current-state :macro-undo-map #(merge-with into % processed-file))]
               ;(ut/pp [:atom-updated (:macro-undo-map new-state)])
               new-state)))))

(defn process-existing-files [dir-path]
  ;(ut/pp [:processing-existing-files dir-path])
  (let [dir (io/file dir-path)]
    (if (.exists dir)
      (if (.isDirectory dir)
        (let [files (file-seq dir)]
          ;(ut/pp [:found-files (count files)])
          (doseq [file files
                  :when (.isFile file)
                  :let [file-path (.getPath file)]
                  :when (or (cstr/ends-with? file-path ".edn")
                            (cstr/ends-with? file-path ".jpg"))]
            (update-macro-undo-map file-path)
            ;(ut/pp [:existing-file-processed file-path])
            ))
        (ut/pp [:error-not-a-directory dir-path]))
      (ut/pp [:error-directory-not-found dir-path]))))

(defn watch-macro-undos []
  ;(ut/pp [:starting-watch-macro-undos])
  (let [relative-path "assets/snaps/"
        file-path (-> relative-path io/file .getAbsolutePath)]
    ;(ut/pp [:watching-directory file-path])
    (process-existing-files file-path)
    ;(ut/pp [:initial-macro-undo-map (get @db/params-atom :macro-undo-map)])
    (beholder/watch
     (fn [event]
       ;(ut/pp [:beholder-event event])
       (let [changed-path (str (:path event))]
         (when (or (cstr/ends-with? changed-path ".edn")
                   (cstr/ends-with? changed-path ".jpg"))
           (update-macro-undo-map changed-path)
           ;(ut/pp [:macro-undo-map-updated (get @db/params-atom :macro-undo-map)])
           )))
     file-path)))

(defn watch-file [filepath on-change-fn] ;; time generalize this shiz. doesnt work?
  (let [file (io/file filepath)
        abs-path (.getAbsolutePath file)]
    (if (.exists file)
      (do
        (ut/pp [:watch-file abs-path])
        (beholder/watch (fn [event] (on-change-fn event)) abs-path))
      (ut/pp [:watch-file "File or directory not found:" abs-path]))))

;; :client/macro-undo-map>client-name
;; (ut/pp [:watch-macro-undos (get @db/params-atom :macro-undo-map)])
;; (ut/pp [:watch-macro-undos (keys @db/params-atom )])

(defn watch-solver-files []
  (let [file-path "./defs/"]
    (beholder/watch #(ppy/execute-in-thread-pools
                      :watch-solver-files-serial
                      (fn [] (when (and
                                    (or (cstr/ends-with? (str (get % :path)) "signals.edn")
                                        (cstr/ends-with? (str (get % :path)) "solvers.edn"))
                                    (not @wss/shutting-down?))
                               (let [signals? (cstr/ends-with? (str (get % :path)) "signals.edn")
                                     destinations (vec (keys @wss/client-queues))
                                     map-atom (if signals? wss/signals-atom wss/solvers-atom)
                                     _ (reset! map-atom (edn/read-string (slurp (str (get % :path)))))
                                     _ (ut/pp [(if signals? :signals :solvers) :file-change! signals? (get % :path)])
                                     _ (if signals?
                                         (wss/reload-signals-subs)
                                         (wss/reload-solver-subs))]
                                 (doseq [d destinations]
                                   (wss/alert! d
                                               [:v-box :justify :center :style {:opacity 0.7} :children
                                                [[:box :style {:color :theme/editor-outer-rim-color :font-weight 700}
                                                  :child (str "Note: Server " (if signals? "signals" "solvers") " have been updated & received")]
                                                 [:box :child (str (get % :path))]]]
                                               13 2
                                               5)
                                   (wss/kick d [(if signals? :signals-file :solvers-file)] @map-atom 1 :none (str "file updated " (get % :path))))))))
                    file-path)))

(defn start-services []
  (ut/pp [:starting-services...])
  (evl/create-nrepl-server!) ;; needs to start
  (update-all-screen-meta) ;; has queue per screen
  (update-all-flow-meta) ;; has queue per flow
  (update-all-conn-meta)
  (wss/reload-signals-subs) ;; run once on boot
  (wss/reload-solver-subs) ;; run once on boot (needs nrepl(s) to start first...)
  (wss/create-web-server!)
  (let [flows (vec (filter #(not (cstr/includes? (str %) "-solver-flow-")) (keys @flow-db/results-atom)))]
    (reset! flow-db/results-atom (select-keys (ut/replace-large-base64 @flow-db/results-atom) flows))))

(defonce scheduled-tasks (atom {}))

(defn start-scheduler [secs f name-str & [delay]]
  (ut/pp ["Starting scheduler" name-str "every" secs "seconds"])
  (let [delay (or delay 0)
        task (.scheduleAtFixedRate ppy/general-scheduler-thread-pool
                                   (reify Runnable
                                     (run [_]
                                       (try
                                         (f)
                                         (swap! wss/scheduler-atom assoc name-str (merge
                                                                                   {:last-run (ut/get-current-timestamp)
                                                                                    :times-run (inc (get-in @wss/scheduler-atom [name-str :times-run] 0))}
                                                                                   (dissoc (get @wss/scheduler-atom name-str) :last-run :times-run)))
                                         (catch Exception e
                                           (ut/pp [e "Error in scheduler" name-str])))))
                                   delay
                                   secs
                                   TimeUnit/SECONDS)]
    (swap! scheduled-tasks assoc name-str task)))

(defn stop-all-schedulers []
  (ut/pp ["Stopping all schedulers"])
  (doseq [[name task] @scheduled-tasks]
    (ut/pp ["Stopping scheduler" name])
    (.cancel task true))
  (reset! scheduled-tasks {}))

;; (defn reboot-websocket-server-long []
;;   (let [destinations (vec (keys @wss/client-queues))]
;;     (doseq [d destinations]
;;       (wss/alert! d
;;                   [:v-box
;;                    :justify :center
;;                    :style {:opacity 0.7}
;;                    :children [[:v-box :style {:color :theme/editor-outer-rim-color :font-weight 700 :font-size "14px"}
;;                                :children [[:box :child (str "Restarting Websocket Server...")]
;;                                           [:progress-bar [280 150 (str (rand-int 12345))]]]]]]
;;                   10 1
;;                   160)))
;;   (Thread/sleep 10000)
;;   (wss/destroy-websocket-server!)
;;   (Thread/sleep 120000)
;;   (reset! wss/websocket-server (jetty/run-jetty #'wss/web-handler wss/ring-options)))

(defn reboot-reactor-and-resub []
    ;(evl/graceful-restart-nrepl-server!)
    ;(Thread/sleep 10000)
  (db/reboot-reactor!)
    ;(Thread/sleep 10000)
  (wss/reload-signals-subs)
  (wss/reload-solver-subs))

(def last-look (atom {}))
(def saved-uids (atom []))

;;  (sql-exec cache-db "drop table if exists jvm_stats;")

(defn update-stat-atom [kks]
  (qp/serial-slot-queue :update-stat-atom-serial :serial
    ;(ppy/execute-in-thread-pools :update-stat-atom
                        (fn []
                          (let [;flows (if (or (nil? flows) (empty? flows))
                                ]
                            (swap! db/flow-status merge
                                   (into
                                    {}
                                    (for [k     kks ;(keys @flow-db/results-atom)
                                          :when (not (= k "client-keepalive"))] ;; dont want to track heartbeats
                                      (let [cc              (into {} (for [[k v] @flow-db/results-atom] {k (count v)})) ;; wtf?
                                            blocks          (get-in @flow-db/working-data [k :components-list])
                                            running-blocks  (vec (for [[k v] (get @flow-db/tracker k) :when (nil? (get v :end))] k))
                                            done-blocks     (vec (for [[k v] (get @flow-db/tracker k) :when (not (nil? (get v :end)))] k))
                                            not-started-yet (vec (cset/difference (set blocks) (set (into running-blocks done-blocks))))
                                            running-blocks  (vec (cset/difference (set blocks) (set (into done-blocks not-started-yet))))
                                            res             (get-in @flow-db/results-atom [k :done] :nope)
                                            running?        (ut/ne? running-blocks) ;;(and (or (= res :nope) (ut/chan? res) (= res :skip)) (ut/ne? running-blocks))
                                            done?           (and (empty? not-started-yet) (empty? running-blocks)) ;;(and (empty? running-blocks) (not (= res :nope)))
                                            error?          (try (some #(or (= % :timeout) (= % :error)) (ut/deep-flatten res)) (catch Exception _ false))
                                              ;;;_ (ut/pp [:flow-status! k :running? running? :done? done?   (get-in @flow-db/tracker [k :done :end])  ])
                                            _ (when error? (swap! wss/temp-error-blocks assoc k not-started-yet))
                                            start           (try (apply min (or (for [[_ v] (get @flow-db/tracker k)] (get v :start)) [-1]))
                                                                 (catch Exception _ (System/currentTimeMillis)))
                                            start-ts        (ut/millis-to-date-string start)
                                            end             (try (apply max (or (remove nil? (for [[_ v] (get @flow-db/tracker k)] (get v :end))) [-1]))
                                                                 (catch Exception e
                                                                   (do ;(ut/pp [:exception-in-getting-time-duration!! k
                                                                     (System/currentTimeMillis))))
                                            start           (if error? (get-in @wss/last-times [k :start]) start)
                                            end             (if error? (System/currentTimeMillis) end)
                                            elapsed         (try (- end start)
                                                                 (catch Throwable e (do (ut/pp [:elapsed-error?? k e :start start :end end]) 0)))
                                            _ (swap! wss/last-times assoc-in [k :end] (System/currentTimeMillis))
                                            human-elapsed   (ut/format-duration start end)
                                            run-id          (str (get-in @flow-db/results-atom [k :run-id]))
                                            orig-flow-id    (str (get-in @flow-db/results-atom [k :opts-map :orig-flow-id] k))
                                            parent-run-id   (str (get-in @flow-db/results-atom [k :parent-run-id]))
                                            overrides       (get-in @flow-db/subflow-overrides [k run-id])
                                            cname           (get-in @flow-db/results-atom
                                                                    [k :opts-map :client-name]
                                                                    (get @wss/orig-caller k :rvbbit))
                                            client-name     (str cname)
                                            run-sql?        (and done? (not (some #(= % run-id) @saved-uids)))
                                            _ (when (and done? (not error?))
                                                (swap! wss/times-atom assoc k (conj (get @wss/times-atom k []) (int (/ elapsed 1000)))))
                                            _ (when run-sql?
                                                (try (let [row        {:client_name     client-name
                                                                       :flow_id         (str k)
                                                                       :started         start ;(get-in @flow-db/results-atom
                                                                       :start_ts        start-ts
                                                                       :ended           end ;(get-in @flow-db/results-atom
                                                                       :run_id          run-id
                                                                       :parent-run_id   parent-run-id
                                                                       :orig_flow_id    orig-flow-id
                                                                       :elapsed         elapsed
                                                                       :overrides       (pr-str overrides)
                                                                       :elapsed_seconds (float (/ elapsed 1000))
                                                                       :in_error        (cstr/includes? (str res) ":error")
                                                                       :human_elapsed   (str human-elapsed)}
                                                           insert-sql {:insert-into [:flow_history] :values [row]}]
                                                       (sql-exec systemh2-db (to-sql insert-sql))) ;; sql-exec has it's own serial queue
                                                     (catch Exception e (ut/pp [:flow-history-insert-error k (str e)]))))
                                            _ (swap! last-look assoc k done?)
                                            _ (when run-sql? (swap! saved-uids conj run-id))
                                            chans-open      (count (doall (map (fn [[_ ch]]
                                                                                 (let [vv (try (not (ut/channel-open? ch)) (catch Throwable e (str e)))]
                                                                                   (if (cstr/includes? (str vv) "put nil on channel") :open vv)))
                                                                               (get @flow-db/channels-atom k))))
                                            channels-open?  (true? (> chans-open 0))]
                                        {k {:*done?          done? ;(true? (not (nil? res)))
                                            :*started-by     cname
                                            :*channels-open? channels-open?
                                            :*channels-open  chans-open
                                            :running-blocks  running-blocks
                                            :done-blocks     done-blocks
                                            :waiting-blocks  not-started-yet
                                            :error-blocks    (get @wss/temp-error-blocks k [])
                                            :overrides       overrides
                                            :started         start
                                            :*finished       (count done-blocks)
                                            :*running        running-blocks
                                            :*done           done-blocks
                                            :*ms-elapsed     elapsed
                                            :*time-running   (str human-elapsed)
                                            :*not-started    not-started-yet
                                            :*open-channels  (get cc k)
                                            :*running?       running?}}))))))))

(defn tracker-changed [_ _ old-state new-state]
  (when (not= old-state new-state)
    (let [[_ b _] (data/diff old-state new-state)
          kks     (try (keys b) (catch Exception _ []))
          kks     (vec (remove #(= "client-keepalive" %) kks))
            ;kks     (remove #(= "client-keepalive" %) kks)
          ]
      (when (> (count kks) 0)
        (update-stat-atom kks)))))

(defn log-tracker [kks]
  (doseq [flow-id kks]
    (let [orig-tracker (get @flow-db/tracker flow-id)
          tracker      (into {}
                             (for [[k v] orig-tracker ;; remove condi non-starts. dont send
                                   :when (not (get v :in-chan?))]
                               {k v}))]
      (swap! wss/tracker-history assoc flow-id (vec (conj (get @wss/tracker-history flow-id []) tracker))))))

(defn combined-tracker-watcher [key ref old-state new-state]
  (when (not= new-state old-state)

     ; (qp/serial-slot-queue :combined-tracker-watcher-serial :serial-queues
    (ppy/execute-in-thread-pools
     :combined-tracker-watcher-side-effects
     (fn []

       (let [[_ b _] (data/diff old-state new-state)
             kks     (try (keys b) (catch Exception _ nil))]
         (when (> (count (remove #(= "client-keepalive" %) kks)) 0)
      ;; Side effects part (originally from tracker-changed2)
           (ppy/execute-in-thread-pools :flow-log-file-writing
                                        (fn []
                                          ;(ext/create-dirs "./flow-logs/")
                                          (doseq [flow-id kks
                                                  :let [run-id     (str (get-in @flow-db/results-atom [flow-id :run-id]))
                                                        fp         (str "./flow-logs/" (cstr/replace flow-id "/" "-CALLING-") "--" run-id ".edn")
                                                        mst        (System/currentTimeMillis)
                                                        _          (swap! wss/watchdog-atom assoc flow-id mst)
                                                        diffy      (get (ut/replace-large-base64 b) flow-id)
                                                        diffy-keys (into {} (for [[k v] diffy] {k (first (keys v))}))
                                                        _          (swap! wss/last-block-written assoc flow-id diffy-keys)
                                                        blocks     (keys (get (ut/replace-large-base64 b) flow-id))]]
                                            (let [data        [[(ut/millis-to-date-string mst) blocks]
                                                               {:values (select-keys (ut/replace-large-base64 (get-in @flow-db/results-atom [flow-id])) blocks)
                                                                :diff   diffy}]
                                                  pretty-data (with-out-str (ppt/pprint data))]
                                              (spit fp (str pretty-data "\n") :append true)))))
           (log-tracker kks)
           (ppy/execute-in-thread-pools
            :runstream-pre-package
            (fn [] (doseq [k kks] (swap! db/runstream-atom assoc k (wss/get-flow-open-ports k k)))))
              ;; get-flow-open-ports [flowmap flow-id client-name]
           (update-stat-atom kks)))))))

(defn heartbeat [dest] ;; flowmaps flow for websocket client heartbeats
  {:components {:kick-1 {:inputs [:destination :name :sub-task :thread-id :thread-desc :message-name]
                         :fn     '(fn
                                    [destination name sub-task thread-id thread-desc message-name & args]
                                    (rvbbit-backend.websockets/kick
                                     destination
                                     name
                                     sub-task
                                     thread-id
                                     thread-desc
                                     message-name
                                     args))
                         :raw-fn '(fn
                                    [destination name sub-task thread-id thread-desc message-name & args]
                                    (rvbbit-backend.websockets/kick
                                     destination
                                     name
                                     sub-task
                                     thread-id
                                     thread-desc
                                     message-name
                                     args))}
                :open-input        [:heartbeat]
                :kick-1destination dest}
   :connections [[:kick-1 :done]
                 [:open-input :kick-1/name]
                 [:open-input :kick-1/sub-task]
                 [:open-input :kick-1/thread-id]
                 [:open-input :kick-1/thread-desc]
                 [:open-input :kick-1/message-name]
                 [:kick-1destination :kick-1/destination]]})



  ;; (time
  ;;  (cruiser/lets-give-it-a-whirl "connections/boston-crime.edn"
  ;;                                       (get @db/conn-map "boston-crime")
  ;;                                       system-db
  ;;                                       cruiser/default-sniff-tests
  ;;                                       cruiser/default-field-attributes
  ;;                                       cruiser/default-derived-fields
  ;;                                       cruiser/default-viz-shapes
  ;;                                [:= :table-name :offenses]
  ;;                                ))
  ;; (time
  ;;  (ut/pp [:shape-rotator-fields
  ;;          (count (rota/get-field-maps {:f-path "connections/boston-crime.edn"
  ;;                                       :shape-test-defs cruiser/default-sniff-tests
  ;;                                       :shape-attributes-defs cruiser/default-field-attributes
  ;;                                       :filter-fn nil ;(fn [m] (true? (get m :very-low-cardinality?)))
  ;;                                       :pre-filter-fn nil}))]))





(defn -main [& args]
  (let [] ;; [xtdb-node (xtn/start-node)]

    ;; (defonce xtdb-node (xtn/start-node
    ;;                     ;; {:log     [:local {:path "db/xtdb-system/tx-log"}] ;; <-- omit and use memory trans log?
    ;;                     ;;  :storage [:local {:path "db/xtdb-system/storage"}]
    ;;                     ;;  :server [:local {:port 3000}]
    ;;                     ;;  :pgwire [:local {:port 5432}]
    ;;                     ;;  }

    ;;                     ;; {:xtdb.log/local-directory-log {:root-path "/var/lib/xtdb/log"}
    ;;                     ;;  :xtdb.buffer-pool/local {:data-dir "/var/lib/xtdb/buffers"}
    ;;                     ;;  :xtdb/server {:port 3000}
    ;;                     ;;  :xtdb/pgwire {:port 5432}
    ;;                     ;;  :xtdb.flight-sql/server {:host "0.0.0.0", :port 9832}}
    ;;                     ))
    ;; (ut/pp ["XTDB node started successfully" (xt/status xtdb-node)])

    (println " ")
    (ut/print-ansi-art "data/nname.ans")
    (ut/print-ansi-art "data/rrvbbit.ans")
    (ut/pp [:version "0.2.0" :november 2024 "Yo."])
    ;; (ut/pp [:pre-alpha "lots of bugs, lots of things to do - but, and I hope you'll agree.. lots of potential."])
    (ut/pp ["Ryan Robitaille" "@ryrobes" ["rvbbit.com" "ryrob.es"] "ryan.robitaille@gmail.com"])
  ;; (println " ")
  ;; (wss/fig-render "Curiouser and curiouser!" :pink)

   ;; create dirs for various artifacts, if not already present
    (doseq [dir ["assets" "assets/snaps" "assets/screen-snaps" "defs/backup"
                 ;; "db/xtdb-system/tx-log" "db/xtdb-system/storage"
                 "flow-logs" "flow-blocks" "flow-history" "live"]]
      (ext/create-dirs dir))

  ;;  (xtdb.api/start-node {:log [:local {:path "db/xtdb-system/tx-log"
  ;;                                      ;; accepts `java.time.InstantSource`
  ;;                                      ;; :instant-src (InstantSource/system)
  ;;                                      ;; :buffer-size 4096
  ;;                                      ;; :poll-sleep-duration "PT1S"
  ;;                                      }]
  ;;                        ;:modules [:httpserver {:port 3000}]
  ;;                        :storage [:local
  ;;                                  {:path "db/xtdb-system/storage"
  ;;                                    ;; :max-cache-bytes 1024
  ;;                                    ;; :max-cache-entries 536870912
  ;;                                   }]})

    (shell/sh "/bin/bash" "-c" (str "rm -rf " "live/*"))
    (get-ai-workers) ;; just in case for the cold boot atom has no

    (qp/create-slot-queue-system)
    (fpop/thaw-flow-results)

    (ut/pp [:creating-h2-tables "systemh2-db"])
    ;; (sql-exec systemh2-db "drop table fields;")
    (sql-exec systemh2-db h2/create-screens)
    (sql-exec systemh2-db h2/create-blocks)
    (sql-exec systemh2-db h2/create-realms)
    (sql-exec systemh2-db h2/create-fields)
    (sql-exec systemh2-db h2/create-fields-view)
    (sql-exec systemh2-db h2/create-panel-history)
    (sql-exec systemh2-db h2/create-panel-materialized-history)
    (sql-exec systemh2-db h2/create-panel-resolved-history)
    (sql-exec systemh2-db h2/create-client-memory)
    (sql-exec systemh2-db h2/create-client-stats)
    (sql-exec systemh2-db h2/create-jvm-stats)
    (sql-exec systemh2-db h2/create-client-items)
    (sql-exec systemh2-db h2/create-flow-results)
    (sql-exec systemh2-db h2/create-flow-schedules)
    (sql-exec systemh2-db h2/create-flows)
    (sql-exec systemh2-db h2/create-flow-history)
    (sql-exec systemh2-db h2/create-live-schedules)
    (sql-exec systemh2-db h2/create-channel-history)
    (sql-exec systemh2-db h2/create-fn-history)
    ;; (sql-exec systemh2-db h2/create-ft-index)

    (db/open-ddb "honeyhash-map") ;; start datalevin "table" instances
    (db/open-ddb "shapes-map")
    ;(db/open-ddb "shapes-map/fields")
    ;(db/open-ddb "shapes-map/shapes")
    (db/open-ddb "last-values")

    (sql-exec system-db "drop table if exists jvm_stats;")
    (sql-exec system-db "drop table if exists client_memory;")
    (sql-exec system-db "drop table if exists errors;")
    (sql-exec system-db "drop view  if exists viz_recos_vw;")
    (sql-exec system-db "drop view  if exists viz_recos_vw2;")
    (sql-exec system-db "drop view  if exists user_subs_vw;")
    (sql-exec system-db "drop view  if exists latest_status;")

    (cruiser/create-sqlite-sys-tables-if-needed! system-db)
    ;; (cruiser/create-sqlite-flow-sys-tables-if-needed! flows-db)
    (cruiser/create-system-reporting-tables-if-needed! system-reporting-db)

    ;; (cruiser/create-sqlite-sys-tables-if-needed! cruiser/tmp-db-dest1)
    ;; (cruiser/create-sqlite-sys-tables-if-needed! cruiser/tmp-db-dest2)
    ;; (cruiser/create-sqlite-sys-tables-if-needed! cruiser/tmp-db-dest3)
    ;; (cruiser/create-sqlite-sys-tables-if-needed! systemh2-db)

    (merge-shape-files) ;; build db/shapes-map for upcoming shape-rotation

    (when harvest-on-boot?
      (ppy/execute-in-thread-pools :shape-rotation-jobs (fn [] (update-all-conn-meta))))

    #_{:clj-kondo/ignore [:inline-def]}
    (defonce start-shape-watcher (watch-shapes-folder))
    #_{:clj-kondo/ignore [:inline-def]}
    (defonce start-conn-watcher (watch-connections-folder))
    #_{:clj-kondo/ignore [:inline-def]}
    (defonce start-screen-watcher (watch-screens-folder))
    #_{:clj-kondo/ignore [:inline-def]}
    (defonce start-flow-watcher (watch-flows-folder))
    #_{:clj-kondo/ignore [:inline-def]}
    (defonce start-settings-watcher (watch-config-files))
    #_{:clj-kondo/ignore [:inline-def]}
    (defonce start-solver-watcher (watch-solver-files))
    ;; #_{:clj-kondo/ignore [:inline-def]}
    ;; (defonce session-file-watcher (wss/subscribe-to-session-changes)) ;; stopping for now. not being used at all.
    #_{:clj-kondo/ignore [:inline-def]}
    (defonce macro-undo-watcher (watch-macro-undos))
    #_{:clj-kondo/ignore [:inline-def]}
    (defonce start-ai-workers-watcher (watch-ai-workers-folder))
    #_{:clj-kondo/ignore [:inline-def]}
    ;;(defonce watch-leaves-watcher (watch-file "defs/leaves.edn" leaves/clear-all-leaf-caches))

  ;; temp test indexes
  ;; (ut/pp (sql-exec system-db "CREATE INDEX idx_client_name ON client_memory(client_name);"))
  ;; (ut/pp (sql-exec system-db "CREATE INDEX idx_ts ON client_memory(ts);"))
  ;; (ut/pp (sql-exec system-db "CREATE INDEX idx_client_name2 ON client_memory(client_name, ts);"))
  ;; (ut/pp (sql-exec system-db "CREATE INDEX idx_ts1 ON jvm_stats(ts);"))

  ;; RABBIT REACTOR
  ;; boot master watchers for defined atoms
    (doseq [[type atom] db/master-reactor-atoms]
      (db/master-watch-splitter-deep type atom))

    (ppy/add-watch+ wss/signals-atom
                    :master-signal-def-watcher ;; watcher signals defs
                    (fn [_ _ old-state new-state]
                      (ut/pp [:signals-defs-changed :reloading-signals-sys-subs....])
                      (wss/reload-signals-subs))
                    :master-signal-def-watcher)

    ;; (ppy/add-watch+ wss/rules-atom
    ;;                 :master-rule-def-watcher
    ;;                 (fn [_ _ old-state new-state]
    ;;                   (ut/pp [:rules-defs-changed :reloading....]))
    ;;                 :master-rule-def-watcher)

    (ppy/add-watch+ wss/solvers-atom
                    :master-solver-def-watcher
                    (fn [_ _ old-state new-state]
                      (ut/pp [:solvers-defs-changed :reloading....])
                      (wss/reload-solver-subs))
                    :master-solver-def-watcher)

    ;; (cruiser/insert-current-rules!
    ;;  system-db
    ;;  "system-db"
    ;;  0
    ;;  cruiser/default-sniff-tests
    ;;  cruiser/default-field-attributes
    ;;  cruiser/default-derived-fields
    ;;  cruiser/default-viz-shapes
    ;;  (merge cruiser/default-flow-functions {:custom @wss/custom-flow-blocks} {:sub-flows @wss/sub-flow-blocks}))

    #_{:clj-kondo/ignore [:inline-def]}
    (defonce system-dbs
      [system-db systemh2-db import-db metrics-kpi-db cache-db cache-db-memory system-reporting-db])

    (ppy/execute-in-thread-pools
     :shape-rotation-jobs-wrapper
     (fn [] (doseq [db system-dbs]
              (ppy/execute-in-thread-pools :shape-rotation-jobs (fn [] (shape-rotation-run db))))))

    ;; (time (cruiser/lets-give-it-a-whirl-no-viz
    ;;        "system-db"
    ;;        system-db
    ;;        system-db
    ;;        cruiser/default-sniff-tests
    ;;        cruiser/default-field-attributes
    ;;        cruiser/default-derived-fields
    ;;        cruiser/default-viz-shapes))

    ;; (time (cruiser/lets-give-it-a-whirl-no-viz
    ;;        "flows-db"
    ;;        flows-db
    ;;        system-db
    ;;        cruiser/default-sniff-tests
    ;;        cruiser/default-field-attributes
    ;;        cruiser/default-derived-fields
    ;;        cruiser/default-viz-shapes))

    ;; (time (cruiser/lets-give-it-a-whirl-no-viz
    ;;        "systemh2-db"
    ;;        systemh2-db
    ;;        system-db
    ;;        cruiser/default-sniff-tests
    ;;        cruiser/default-field-attributes
    ;;        cruiser/default-derived-fields
    ;;        cruiser/default-viz-shapes))

    ;; (time (cruiser/lets-give-it-a-whirl-no-viz
    ;;        "import-db"
    ;;        import-db
    ;;        system-db
    ;;        cruiser/default-sniff-tests
    ;;        cruiser/default-field-attributes
    ;;        cruiser/default-derived-fields
    ;;        cruiser/default-viz-shapes))

    ;; (time (cruiser/lets-give-it-a-whirl-no-viz
    ;;        "metrics-kpi-db"
    ;;        metrics-kpi-db
    ;;        system-db
    ;;        cruiser/default-sniff-tests
    ;;        cruiser/default-field-attributes
    ;;        cruiser/default-derived-fields
    ;;        cruiser/default-viz-shapes))

    ;; (cruiser/clean-up-db-metadata "cache.db")
    ;; (time (cruiser/lets-give-it-a-whirl-no-viz
    ;;        "cache.db"
    ;;        cache-db
    ;;        system-db
    ;;        cruiser/default-sniff-tests
    ;;        cruiser/default-field-attributes
    ;;        cruiser/default-derived-fields
    ;;        cruiser/default-viz-shapes))

    ;; (cruiser/clean-up-db-metadata "cache.db.memory")
    ;; (time (cruiser/lets-give-it-a-whirl-no-viz
    ;;        "cache.db.memory"
    ;;        cache-db-memory
    ;;        system-db
    ;;        cruiser/default-sniff-tests
    ;;        cruiser/default-field-attributes
    ;;        cruiser/default-derived-fields
    ;;        cruiser/default-viz-shapes))

    ;; (time (cruiser/lets-give-it-a-whirl-no-viz
    ;;        "realms-db"
    ;;        realms-db
    ;;        system-db
    ;;        cruiser/default-sniff-tests
    ;;        cruiser/default-field-attributes
    ;;        cruiser/default-derived-fields
    ;;        cruiser/default-viz-shapes))

    ;; (time (cruiser/lets-give-it-a-whirl-no-viz
    ;;        "history-db"
    ;;        history-db
    ;;        system-db
    ;;        cruiser/default-sniff-tests
    ;;        cruiser/default-field-attributes
    ;;        cruiser/default-derived-fields
    ;;        cruiser/default-viz-shapes))

    ;; (time (cruiser/lets-give-it-a-whirl-no-viz
    ;;        "system-reporting-db"
    ;;        system-reporting-db
    ;;        system-db
    ;;        cruiser/default-sniff-tests
    ;;        cruiser/default-field-attributes
    ;;        cruiser/default-derived-fields
    ;;        cruiser/default-viz-shapes))

  ;;;  (reboot-reactor-and-resub)

  ;; (start-scheduler 15
  ;;                  #(let [ddiff (- (System/currentTimeMillis)  (get-in @wss/rvbbit-client-sub-values [:unix-ms]))]
  ;;                     (when
  ;;                      (> ddiff 31000)
  ;;                       (ut/pp [:WATCHDOG-REBOOTING-REACTOR-AND-SUBS! ddiff :ms :behind!])
  ;;                       (swap! wss/scheduler-atom assoc :executed! (inc (get-in @wss/scheduler-atom [:executed!] 0)))
  ;;                       (reboot-reactor-and-resub)))
  ;;                  "Watchdog - Reboot Reactor and Subs" 30)

    (start-scheduler 1
                     #(try
                        (let [ddate (ut/current-datetime-parts)
                              stamp (System/currentTimeMillis)
                              ddate (-> ddate
                                        (assoc (keyword (str "second=" (get ddate :second))) stamp)
                                        (assoc (keyword (str "minute=" (get ddate :minute))) stamp)
                                        (assoc (keyword (str "hour=" (get ddate :hour))) stamp))]
                          (reset! db/father-time ddate))
                        (catch Exception e
                          (println "Error updating atom:" (.getMessage e))
                          (throw e)))
                     "Father Time")

    (start-scheduler 15
                     #(try (wss/jvm-stats) (catch Throwable e (ut/pp [:error-in-jvm-stats-fn-sched (.getData e)])))
                     "JVM Stats" 30)

    (start-scheduler 600
                     realms/load-realm-maps
                     "Rebuild the Realms!" 45)

    (start-scheduler 600
                     #(reset! db/query-runstream-cache {})
                     "Clear query runstream cache!" 45)

    ;; (start-scheduler 7
    ;;                  #(do (leaves/leaf-eval-runstream :fields)
    ;;                       (leaves/leaf-eval-runstream :views))
    ;;                  "Resolving View Leaves" 45)

    (start-scheduler 15
                     #(wss/flow-statuses true)
                     "Flow Stats & Watchdog" 15)

    ;; (start-scheduler 15
    ;;                  #(ut/pp [:xtdb-node-status (xt/status xtdb-node)])
    ;;                  "XTDB Node Status (debug)" 45)

    ;; (start-scheduler 240 ;; was 30
    ;;                  #(ppy/execute-in-thread-pools
    ;;                    :param-sql-sync
    ;;                    (fn []
    ;;                      (cruiser/lets-give-it-a-whirl-no-viz
    ;;                       "cache.db"
    ;;                       cache-db
    ;;                       system-db
    ;;                       cruiser/default-sniff-tests
    ;;                       cruiser/default-field-attributes
    ;;                       cruiser/default-derived-fields
    ;;                       cruiser/default-viz-shapes)
    ;;                      (wss/param-sql-sync)))
    ;;                  "Parameter Sync" 30)

    (start-scheduler 45
                     #(when (> (count (db/client-subs-late-delivery 30000)) 0)
                        (wss/sync-client-subs))
                     "Sync Client Subs" 120)

    ;; (start-scheduler 120
    ;;                  #(doseq [[name conn] @sql/client-db-pools]
    ;;                     (qp/serial-slot-queue
    ;;                      :re-sniff-client-sql-dbs :single
    ;;                      (fn []
    ;;                        (time (cruiser/lets-give-it-a-whirl-no-viz
    ;;                         (cstr/replace (str name) ":" "")
    ;;                         {:datasource conn}
    ;;                         system-db
    ;;                         cruiser/default-sniff-tests
    ;;                         cruiser/default-field-attributes
    ;;                         cruiser/default-derived-fields
    ;;                         cruiser/default-viz-shapes)))))
    ;;                  "(Re)Sniff Client SQL DBs" 120)

    ;; (start-scheduler 30
    ;;                #(qp/serial-slot-queue
    ;;                  :re-sniff-cache-db :single
    ;;                  (fn []
    ;;                    (cruiser/lets-give-it-a-whirl-no-viz
    ;;                     "cache.db"
    ;;                     wss/cache-db
    ;;                     system-db
    ;;                     cruiser/default-sniff-tests
    ;;                     cruiser/default-field-attributes
    ;;                     cruiser/default-derived-fields
    ;;                     cruiser/default-viz-shapes)))
    ;;                "(Re)Sniff Cache SQL DB" 30)

    (start-scheduler 600 ;; sketch for little gain?
                     db/clean-up-reactor
                     "Remove unneeded watchers from Reactor" 120)

    (start-scheduler 300 ;; was 5
                     #(ppy/execute-in-thread-pools
                       :flow-atoms>sql
                       (fn [] (wss/flow-atoms>sql)))
                     "Refresh Flow Tables" 120)

    (start-scheduler 600 ;; 10 minutes
                     wss/purge-dead-client-watchers
                     "Purge Dead Clients" 720)

    (start-scheduler 3600 ;; 1 hour
                     #(clear-all-leaf-caches "leave caches cleared...")
                     "Clear Leaf Eval Cache Atoms" 720)

  ;; (start-scheduler (* 3600 3) ;; 3 hours (causes big cpu jump whne lots of clients present?)
  ;;                  reboot-reactor-and-resub
  ;;                  "Reboot Reactor, Subs, Pools" (* 3600 3))

  ;; (start-scheduler (* 3600 3)
  ;;                  db/pool-cleaner!
  ;;                  "Pool Cleaner" (* 3600 3))

  ;; (start-scheduler 6000 ;; 10 mins - was causing problems?? TODO: investigate, not critical, we barely use the queues except for sqlite writes
  ;;                  #(qp/cleanup-inactive-queues 10) ;; MINUTES
  ;;                  "Purge Idle Queues" 7200)

;; (qp/cleanup-inactive-queues 0)

  ;;(when debug?
    (start-scheduler 1
                     #(do (swap! wss/peer-usage conj (count @wl/sockets))
                          (swap! wss/push-usage conj @wss/all-pushes)
                          (swap! wss/viz-reco-usage conj @db/viz-reco-sniffs)
                          (swap! wss/leaf-eval-usage conj @db/leaf-evals)
                          (swap! wss/reaction-usage conj @wss/sent-reactions)
                          (swap! wss/signal-reaction-usage conj @wss/sent-signal-reactions)
                          (swap! wss/solver-usage conj @wss/solvers-run)
                          (swap! wss/queue-tasks conj @qp/queue-tasks-run)
                          (swap! wss/nrepl-intros-usage conj @evl/nrepls-intros-run)
                          (swap! wss/sql-exec-usage conj @sql/sql-exec-run)
                          (swap! wss/sql-query-usage conj @sql/sql-queries-run)
                          (swap! wss/nrepl-usage conj @evl/nrepls-run)
                          (swap! wss/flow-usage conj @wss/flows-run)
                          (swap! wss/clover-params-usage conj @wss/param-sql-sync-rows)
                          (swap! wss/watcher-usage conj (count (db/current-watchers)))
                          (swap! wss/sub-usage conj (count (db/current-subs)))
                          (swap! wss/sub-client-usage conj (count (db/current-all-subs)))
                          (swap! db/mem-usage conj (ut/memory-used))
                          (let [thread-mx-bean (java.lang.management.ManagementFactory/getThreadMXBean)
                                thread-count (.getThreadCount thread-mx-bean)]
                            (swap! wss/thread-usage conj thread-count))
                          (swap! wss/pool-task-usage conj @ppy/pool-tasks-run)
                          (swap! wss/pool-tasks conj (wss/pool-tasks-count))
                          (swap! wss/sys-load conj (ut/get-system-load-average))
                          (swap! wss/non-heap-mem-usage conj (ut/get-non-heap-memory-usage))
                         ;(swap! wss/time-usage conj (System/currentTimeMillis)) ;; in case we want to easily ref w/o generating
                        ;(qp/update-queue-stats-history) ;; moved to 15 second scheduler
                        ;(qp/update-specific-queue-stats-history [:watcher-to-client-serial :update-stat-atom-serial])
                          (swap! db/cpu-usage conj (ut/get-jvm-cpu-usage)))
                     "Stats Keeper");;)

    (start-scheduler (* 3600 1) ;; 1 hours
                     #(do
                        (ut/pp [:CLEARING-OUT-DEEP-FLATTEN-CACHE]) ;; (ut/pp (ut/calculate-atom-size :current-size wss/solvers-cache-atom)) ;; super expensive on big atoms
                      ;(ut/pp (sql-exec system-db "delete from client_memory where 1=1;"))
                      ;(ut/pp (sql-exec system-db "delete from jvm_stats where 1=1;"))
                        (reset! wss/solvers-cache-hits-atom {})
                        (reset! wss/solvers-cache-atom {})
                        (reset! sql/sql-query-log [])
                        (reset! sql/sql-exec-log [])
                        (reset! rota/sql-query-cache {})
                        (reset! rota/field-maps-cache {})
                        (reset! leaves/keypaths-cache {})
                        (reset! leaves/visible-views-cache {})
                        (reset! leaves/context-map-cache {})
                        (reset! wss/qcache {}) ;; sql result cache
                        (reset! sql/errors [])
                        (reset! cruiser/mutate-fn-cache {})
                        (reset! wss/agg-cache {})
                        (reset! ut/df-cache {}))
                     "Purge Solver & Deep-Flatten Cache" (* 3600 1))

  ;; (ut/calculate-atom-size :current-size ut/df-cache)

  ;; (start-scheduler 1
  ;;                  #(doseq [[client-name solvers] @wss/solver-status]
  ;;                     (doseq [[solver-name v] solvers
  ;;                             :when (get v :running?)]
  ;;                       (swap! wss/solver-status assoc-in
  ;;                              [client-name solver-name :time-running]
  ;;                              (ut/format-duration (get v :started) (System/currentTimeMillis)))))
  ;;                  "Update Solver Statuses")

  ;; (start-scheduler (* 3600 4) ;; every 4 hours, after 6 hours
  ;;                  #(do
  ;;                     (let [destinations (vec (keys @wss/client-queues))]
  ;;                       (doseq [d destinations]
  ;;                         (wss/alert! d
  ;;                                     [:v-box
  ;;                                      :justify :center
  ;;                                      :style {:opacity 0.7}
  ;;                                      :children [[:box :style {:color :theme/editor-outer-rim-color :font-weight 700}
  ;;                                                  :child [:box :child (str "Restarting Websocket Server. See you in ~45 seconds...")]]]]
  ;;                                     10 1
  ;;                                     45)))
  ;;                     (Thread/sleep 10000)
  ;;                     (wss/destroy-websocket-server!)
  ;;                     (wss/reboot-websocket-handler!)
  ;;                     ;(ppy/reboot-websocket-thread-pool!)
  ;;                     (Thread/sleep 30000)
  ;;                     (reset! wss/websocket-server (jetty/run-jetty #'wss/web-handler wss/ring-options)))
  ;;                  "Restart Websocket Server, Test Debug" (* 3600 6))

  ;; (start-scheduler 15
  ;;                  #(let [dbs (wss/database-sizes)]
  ;;                     (qp/update-queue-stats-history) ;; has it's own timestamp key
  ;;                     (doseq [[db dbv] dbs]
  ;;                       (swap! wss/sql-metrics assoc db
  ;;                              (conj (get @wss/sql-metrics db [])
  ;;                                    dbv))))
  ;;                  "Simple SQLite Db Stats" 60)

    (when
     (get (config/settings) :log-atom-sizes? false)
      (wss/get-sys-atom-sizes)
      (start-scheduler 15
                       wss/get-sys-atom-sizes
                       "Log Atom Sizes" 15))

    ;; (start-scheduler 15
    ;;                  #(do
    ;;                     ;; (ut/pp [:hb-sent])
    ;;                     (wss/kick :all [:heartbeat] [:heartbeat] [:heartbeat] [:heartbeat] [:heartbeat]))
    ;;                  "Client Heartbeat")

    (start-scheduler 15
                     #(let [pst (wss/query-pool-sizes)]
                        (doseq [pp (keys pst)]
                          (swap! wss/pool-stats-atom assoc pp (conj (get @wss/pool-stats-atom pp []) (get-in pst [pp 1])))))
                     "Query Pool Sizes")

  ;; (start-scheduler 1
  ;;                  #(let [total-updated (reduce ;; run side effects, but return how many are active
  ;;                                        (fn [acc [client-name solvers]]
  ;;                                          (let [updated-count
  ;;                                                (reduce-kv (fn [inner-acc solver-name v]
  ;;                                                             (if (get v :running?)
  ;;                                                               (do ;(swap! wss/solver-status assoc-in ;; causes a lot of reactions... but worth it?
  ;;                                                                   ;       [client-name solver-name :time-running]
  ;;                                                                   ;       (ut/format-duration (get v :started) (System/currentTimeMillis)))
  ;;                                                                   (inc inner-acc))
  ;;                                                               inner-acc))
  ;;                                                           0
  ;;                                                           solvers)]
  ;;                                            (+ acc updated-count)))
  ;;                                        0
  ;;                                        @wss/solver-status)]
  ;;                     (swap! wss/solver-usage conj total-updated))
  ;;                  "Update Solver Statuses")

  ;; (defn stop-all-schedulers []
  ;;   (doseq [scheduler [mon param-sync refresh-flow-tables purge ;timekeeper
  ;;                      cpukeeper pushkeeper peerkeeper
  ;;                      purge-solver-cache solver-statuses]]
  ;;     (ut/pp [:stopping-async-job-scheduler! (str scheduler)])
  ;;     ;;(async/close! scheduler)
  ;;     scheduler))




  ;; create a flow of all the signals and solvers relations (or an attempt to) - just a fun little experiment for now
  ;; (future
  ;;   (let [signals  (vec (keys @wss/signals-atom))
  ;;         signalsv (vec (apply concat
  ;;                              (for [[k v] @wss/signals-atom]
  ;;                                (for [e (filter #(and (keyword? %) (or (some (fn [x] (= x %)) signals) (cstr/includes? (str %) "/")))
  ;;                                                (ut/deep-flatten (get v :signal)))]
  ;;                                  [e (keyword (str "signal/" (cstr/replace (str k) ":" "")))]))))
  ;;         solvers  (vec (for [[k v] @wss/solvers-atom
  ;;                             :when (keyword? (get v :signal))]
  ;;                         [(get v :signal) (keyword (str "solver/" (cstr/replace (str k) ":" "")))]))
  ;;         conns    (vec (distinct (into signalsv solvers)))
  ;;         fmap     (wss/warren-flow-map conns)]
  ;;     (ut/pretty-spit "./flows/generated-flow-map.edn" fmap)
  ;;   ;;(ut/pp [:warren-flow? conns])
  ;;     ))

    (shutdown/add-hook! ::heads-up-msg #(ut/ppa "Shutting down now!"))

    (shutdown/add-hook! ::the-pool-is-now-closing
                        #(do (reset! wss/shutting-down? true)
                             (db/close-ddb)
                             (doseq [electric-eye [start-conn-watcher start-screen-watcher start-flow-watcher macro-undo-watcher ;;watch-leaves-watcher
                                                   start-ai-workers-watcher start-settings-watcher start-solver-watcher ;session-file-watcher
                                                   ]]
                               (do
                                 (ut/pp [:stopping-beholder-watch (str electric-eye)])
                                 (beholder/stop electric-eye)))
                             (wss/destroy-websocket-server!)
                          ;;  (do (ut/pp [:saving-shutdown-metrics...])
                          ;;      (ext/create-dirs "./shutdown-logs")
                          ;;      (ut/println-to-file (str "./shutdown-logs/" (cstr/replace (ut/get-current-timestamp) " " "_") ".log")
                          ;;                          (fn [] (let [freqs [1 10 30 60 120 240 600]
                          ;;                                       stats [:cpu :threads :mem :sql-queries+ :sql-exec+ :nrepl-calls+ :solvers+ :flows+ :pool-tasks-run+ :workers :msgs+]]
                          ;;                                   (wss/draw-stats      nil freqs true) ;; all stats, with labels
                          ;;                                   (wss/draw-pool-stats nil freqs true)
                          ;;                                   (wss/draw-client-stats nil freqs nil true {:metrics-atom wss/sql-metrics})
                          ;;                                   (wss/draw-client-stats nil freqs nil true)))))
                             (qp/stop-slot-queue-system)
                             (stop-all-schedulers)
                          ;;  (reset! flow-db/results-atom (select-keys @flow-db/results-atom
                          ;;                                            (filter (fn [x] ;; clean up temp runs
                          ;;                                                      (not (or (cstr/includes? (str x) "solver-flow")
                          ;;                                                               (cstr/includes? (str %) "raw"))))
                          ;;                                                    (keys @flow-db/results-atom))))
                          ;;  (let [destinations (vec (keys @wss/client-queues))]
                          ;;    (doseq [d destinations]
                          ;;      (wss/alert! d
                          ;;                  [:v-box
                          ;;                   :justify :center
                          ;;                   :style {:opacity 0.7}
                          ;;                   :children [[:box :style {:color :theme/editor-outer-rim-color :font-weight 700}
                          ;;                               :child [:box :child (str "Heads up: R-V-B-B-I-T system going offline.")]]]]
                          ;;                  10 1
                          ;;                  5)))
                           ;;  (Thread/sleep 2000)
                             (wss/destroy-websocket-server!)
                             (Thread/sleep 2000)
                             (wss/destroy-websocket-server!)))

    (shutdown/add-hook! ::the-pool-is-now-closed
                        #(do
                           (ut/pp [:the-pool-is-now-closing :please-take-your-towels-with-you :have-a-nice-day-citizen!])
                           (doseq [[conn-name conn] @db/conn-map
                                   :let [cn (get conn :datasource)]]
                             (ut/ppa [:shutting-down-connection-pool conn-name cn])
                             (sql/close-pool cn))
                          ;;  (doseq [[conn-name conn] @sql/client-db-pools]
                          ;;    (ut/ppa [:shutting-down-client-connection-pool (cstr/replace (str conn-name) ":" "") conn])
                          ;;    (sql/close-pool conn))
                           ))

    (shutdown/add-hook! ::cleaning-up-after-ai-workers
                        #(do (ut/pp [:cleaning-up-after-ai-workers])
                             (assistants/clean-up-threads-and-assistants)))

    (shutdown/add-hook! ::close-system-pools
                        #(do (wss/destroy-websocket-server!)
                             (Thread/sleep 1000)
                             (wss/stop-web-server!)
                             ;;(.close xtdb-node)
                           ;(wss/stop-worker)
                           ;(wss/stop-worker2)
                           ;(wss/stop-worker3)
                             (ut/ppa [:shutting-down-system-pools])
                             (hik/close-datasource (get system-db :datasource))
                             (wss/destroy-websocket-server!)))

    (shutdown/add-hook! ::clear-cache
                        #(do (ut/ppa [:freezing-system-atoms])
                             (fpop/freeze-atoms)
                             (ut/ppa [:freezing-flow-results-atom])
                             (fpop/freeze-flow-results)
                             (ut/zprint-file "./defs/signals.edn" {:style [:justified-original] :parse-string? true :comment {:count? nil :wrap? nil} :width 120 :map {:comma? false :sort? false}})
                             (ut/zprint-file "./defs/solvers.edn" {:style [:justified-original] :parse-string? true :comment {:count? nil :wrap? nil} :width 120 :map {:comma? false :sort? false}})
                           ;(ut/ppa [:clearing-cache-db])
                           ;(shell/sh "/bin/bash" "-c" (str "rm " "db/cache.db"))
                             (shell/sh "/bin/bash" "-c" (str "rm " "flow-logs/*"))
                             (shell/sh "/bin/bash" "-c" (str "rm " "reaction-logs/*"))
                           ;(shell/sh "/bin/bash" "-c" (str "rm " "status-change-logs/*"))
                             (shell/sh "/bin/bash" "-c" (str "rm " "tracker-logs/*"))
                             (shell/sh "/bin/bash" "-c" (str "rm " "reaction-logs/*"))
                           ;(shell/sh "/bin/bash" "-c" (str "rm " "db/system.db"))
                             ))

    (ppy/add-watch+
     flow-db/status
     :flow-db-status
     tracker-changed
     :flow-db-status)

    (ppy/add-watch+
     flow-db/tracker
     :flow-db-tracker
     combined-tracker-watcher
     :flow-db-tracker)

    (wss/sub-to-value :rvbbit :time/unix-ms true)

  ;; (ut/pp {:settings config/settings})

    (wss/schedule! [:seconds wss/heartbeat-seconds]
                   (heartbeat :all)
                   {:flow-id "client-keepalive" :increment-id? false :close-on-done? true :debug? false})

  ;; (wss/schedule! [:minutes 20]
  ;;                "game-of-life-test1"
  ;;                {:close-on-done? true
  ;;                 :increment-id?  false
  ;;                 :flow-id        "game-of-life-test1"
  ;;                 :debug?         false
  ;;                 :overrides      {:iterations-max 1000 :tick-delay-ms 800}})

  ;; (wss/schedule! [:minutes 30] "counting-loop" {:flow-id "counting-loop" :increment-id? false :close-on-done? true :debug? false})

  ;; (wss/destroy-websocket-server!)
  ;; (reset! wss/websocket-server (jetty/run-jetty #'wss/web-handler wss/ring-options))

    (let [_ (ut/pp [:waiting-for-background-systems...])
          ;;_ (Thread/sleep 6000) ;; 10 enough? want everything cranking before clients come
          _ (reset! wss/websocket-server (jetty/run-jetty #'wss/web-handler wss/ring-options))
          _ (start-services)
          _ (let [destinations (vec (keys @wss/client-queues))]
              (doseq [d destinations]
                (wss/alert! d
                            [:v-box :justify :center :style {:opacity 0.7} :children
                             [[:box :style {:color :theme/editor-outer-rim-color :font-weight 700} :child
                               [:box :child (str "Heads up: R-V-B-B-I-T system online.")]]]]
                            10
                            1
                            5)))]

      ;(Thread/sleep 3000)

      (ut/pp [" GO: end of main-fn "]))))
