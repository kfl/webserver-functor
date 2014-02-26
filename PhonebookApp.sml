(* Simple phonebook with shared state across clients.
   Uses an in-memory "database" implemented as a list. 
*)

structure Database =
struct
   type ('key, 'data) table = ('key * 'data) list
   fun toList x = x
   val empty = []
   fun update(a,b,[])         = [(a,b)]
     | update(a,b,(a1,b1)::t) = if a = a1 then (a,b)::t
                                else (a1,b1)::update(a,b,t)
end

structure PhonebookApp : WebApp =
struct
type state = (string, string) Database.table
val initial = Database.empty

structure HU = HtmlUtils

fun showPhoneBook phonebook =
    ( HU.webpage "Names and numbers"
                 (HU.ul (map (fn (name, no) => name^": "^no)
                             (Database.toList phonebook)))
    , phonebook)

fun main phonebook =
    ( HU.webpage "Phonebook"
                 (concat [ HU.p (HU.link "add" "Add person to phonebook")
                         , HU.p (HU.link "list" "List phonebook")
                         ])
    , phonebook)


fun addPage phonebook =
    ( HU.webpage "Add Person"
                 (HU.form "addperson"
                          [ HU.p ("Name: "^ HU.inputText "name")
                          , HU.p ("Phone number: "^ HU.inputText "number")
                          , HU.p (HU.submit "Insert")
                          ])
    , phonebook)


(* The following three functions deals (rudimentary) with the query
   path of an URL (http://en.wikipedia.org/wiki/Query_string).

   splitArguments: split path into path and query string
   getArgs: transform the fields part of a query string into a list of pairs
   findArg: find the value bound to a given name in the output from getArgs
*)

fun splitArguments path = String.tokens (fn c => c = #"?") path

fun getArgs args =
    map (fn arg => let val [name, value] = String.tokens (fn c => c = #"=") arg
                   in  (name, value)
                   end)
        (String.tokens (fn c => c = #"&") args)

fun findArg name binds = Option.map #2 (List.find (fn(n, _) => name = n) binds)

fun addPerson phonebook args =
    let val bindings = getArgs (hd args)
    in  case (findArg "name" bindings, findArg "number" bindings) of
            (SOME name, SOME number) => (HU.Redirect "/",
                                         Database.update(name, number, phonebook))
         | _ => raise Fail "Wrong input"
    end

fun response (path, phonebook) =
    case splitArguments path of
        "list" :: _ => showPhoneBook phonebook
      | "add" :: _ => addPage phonebook
      | "addperson" :: args => addPerson phonebook args
      | _ => main phonebook

end

structure AppServer = WebServerFct(PhonebookApp)
val _ = AppServer.start(SOME 10001)
