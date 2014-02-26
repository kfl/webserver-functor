structure HtmlUtils =
struct

datatype reply = Html of string
               | Redirect of string



fun webpage title body =
    Html ( String.concatWith "\n"
                             [ "<!doctype html>"
                             , "<html lang=\"da\">"
                             , "<head>"
                             , "<meta charset=\"iso-8859-1\">"
                             , "<title>"^title^"</title>"
                             , "</head>"
                             , "<body>"
                             , body
                             , "</body>"
                             , "</html>"
                             ])


fun tag t cont = concat ["<",t,">", cont, "</", t, ">"]

fun attribs atts = List.concat (map (fn (p, v) => [p, "=","\"", v, "\""]) atts)

fun taga t atts cont =
    concat (["<",t," "] @ attribs atts @ [" >", cont, "</", t, ">"])
fun tage t atts = concat (["<",t," "] @ attribs atts @ [" />"])

val divElem = tag "div"
val p = tag "p"
fun ul items = tag "ul" (String.concatWith "\n" (map (tag "li") items))
fun link url = taga "a" [("href",url)]

fun inputText name = tage "input" [("type", "text"), ("name", name)]
fun submit text = tage "input" [("type", "submit"), ("value", text)]
fun form action cont = taga "form" [("action", action)] (concat cont)



end
