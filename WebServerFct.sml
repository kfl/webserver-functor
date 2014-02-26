(* Simple HTTP server (needs Socket) as a functor.
   Initially based on example code by Peter Sestoft.

   Authors: Ken Friis Larsen (kflarsen@diku.dk) and Peter Sestoft (sestoft@itu.dk)
   Compile:  mosmlc -toplevel HtmlUtils.sml WebServerFct.sml WebApp.sml -o appserver
   Start:    appserver
   Stop:     ctrl-C
*)

signature WebApp =
sig type state
    val initial : state
    val response : string * state -> HtmlUtils.reply * state
end

functor WebServerFct (App : WebApp) :>
        sig type port = int
            val start : port option -> unit
        end =
struct
type port = int

val server  = "Moscow ML HTTP demo app server"
val myaddr  = "0.0.0.0"
val defaultPort = 4242
val admin       = "kflarsen@diku.dk"

fun log msg = print msg;
fun date () = Date.toString(Date.fromTimeUniv(Time.now()))

fun mkheader header status fields mimetype len =
    (log " "; log status; log " "; log (Int.toString len); log "\n";
     concat ([header, "\n", "Date: ", date(), "\n", "Server: ", server, "\n"]
             @ fields
             @ ["Content-type: ", mimetype, "\n",
                "Content-length: ", Int.toString len, "\n\n"]))

val okheader =
    mkheader "HTTP/1.0 200 OK" "200" []
fun movpermheader newpath =
    mkheader "HTTP/1.0 301 Moved permanently" "301"
             ["Location: " ^ newpath ^ "\n"] "text/html"
fun redirectheader newpath =
    mkheader "HTTP/1.0 302 Moved Temporarily" "302"
             ["Location: " ^ newpath ^ "\n"] "text/html"
val forbiddenheader =
    mkheader "HTTP/1.0 403 Forbidden" "403" [] "text/html"
val notfoundheader =
    mkheader "HTTP/1.0 404 Not Found" "404" [] "text/html"
val errorheader =
    mkheader "HTTP/1.0 500 Internal server error" "500" [] "text/html"
val notimplheader =
    mkheader "HTTP/1.0 501 Not implemented" "501" [] "text/plain"

exception Notimplemented

(* fun mkheader header len = *)
(*     let val date = Date.toString(Date.fromTimeUniv(Time.now())) *)
(*     in *)
(* 	concat [header, "\n", *)
(* 		"Date: ", date, "\n", *)
(* 		"Server: ", server, "\n", *)
(* 		"Content-type: text/html\n", *)
(* 		"Content-length: ", Int.toString len, "\n\n"] *)
(*     end *)

(* val okheader    = mkheader "HTTP/1.0 200 OK" *)
(* val errorheader = mkheader "HTTP/1.0 500 Internal server error" *)

datatype reply = datatype HtmlUtils.reply
datatype result = Result of reply | Failure of exn

fun send sock s =
    Socket.sendVec(sock, {buf=Byte.stringToBytes s, ofs=0, size=NONE})

fun senddoc sock header contents =
    let val hdr = header (size contents)
    in print hdr; send sock hdr; send sock contents end

fun addheader sock data =
    case data of
        Result(Html s) => senddoc sock (okheader "text/html") s
      | Result(Redirect location) => senddoc sock (redirectheader location) ""
      | Failure Notimplemented => senddoc sock notimplheader "Only GET request are implemented"
      | Failure e => senddoc sock errorheader
            ("<HTML><HEAD>\n<TITLE>500 Internal Server Error</TITLE></HEAD>\n\
             \<BODY>\n<H1>Internal Server Error</H1>\n\
             \The server could not handle your request.\n\
             \"^exnName e^" "^exnMessage e^"</BODY></HTML>\n")

(* Convert %xy to the character with hex code xy.  No checks done. *)
fun xurldecode s =
    let fun hexval c =
	    if #"0" <= c andalso c <= #"9" then ord c - 48
	    else (ord c - 55) mod 32;
	fun loop []           acc = implode (rev acc)
	  | loop (#"%" :: cr) acc =
	    (case cr of
		 c1 :: c2 :: cr' =>
		     loop cr' (chr (16 * hexval c1 + hexval c2) :: acc)
	       | _ => loop cr (#"%" :: acc))
	  | loop (c :: cr) acc = loop cr (c :: acc)
    in loop (explode s) [] end

fun parseRequest inp  =
    let open Substring
        val _ = log (string (takel (Char.notContains "\r\n") (all inp)))
        val (method, sus1) = splitl Char.isAlpha (all inp)
        val _ = if string method <> "GET" then raise Notimplemented else ()
        val path1 = string (trimr 1 (#1 (position "HTTP" (triml 2 sus1))))
	val path2 = xurldecode path1
    in  path2
    end



fun start portOpt =
    let val sock = Socket.inetStream ()
        val port = Option.getOpt (portOpt, defaultPort)
	val addr = Socket.inetAddr myaddr port
        val buf = Word8Array.array(10000, 0w0)
        fun gethttprequest sock =
            let val got = Socket.recvArr(sock, {buf = buf, ofs=0, size=NONE})
            in  Byte.unpackString(Word8ArraySlice.slice(buf, 0, SOME got))
            end

        fun robustResponse sock state =
            let val (result, newState) = App.response(parseRequest(gethttprequest sock), state)
            in  (Result result, newState)
            end handle e as Fail s => (log("[ Fail " ^ s ^ "]"); (Failure e, state))
                     | Interrupt   => raise Interrupt
                     | e           => (Failure e, state)

        fun next state =
            let val (sock', a) = Socket.accept sock
                val _ = (log (Socket.getinetaddr a); log " ["; log (date()); log "] ")
                val (result, newState) = robustResponse sock' state
	    in  addheader sock' result
              ; Socket.close sock'
              ; next newState
            end
	prim_val catch_interrupt : bool -> unit = 1 "sys_catch_break"
    in
	catch_interrupt true;
	Socket.bind(sock, addr);
	Socket.listen(sock, 150);
	print "Starting HTTP server\n";
        app print ["Go to: http://", myaddr, ":", Int.toString port, " to enjoy it.\n"];
	(next App.initial) handle Interrupt => ();
	print "Shutting down HTTP server\n";
	Socket.close sock
    end
end
