structure CountingEcho : WebApp =
struct

type state = int
val initial = 0

fun response (path, state) =
    let val newState = state+1
    in  ( HtmlUtils.webpage "Counter"
           (concat [ "You gave me the path:"
                   , HtmlUtils.tag "pre" path
                   , HtmlUtils.p ("I've been called "^Int.toString newState^" times.")
           ])
        , newState)
    end
end

structure AppServer = WebServerFct(CountingEcho)
val _ = AppServer.start(SOME 10000)
