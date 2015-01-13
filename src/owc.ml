open Js      ;;
open Base    ;;
open Process ;;

let examples = List.map ((^) "examples/") [ "simple.wc"
                                          ; "first.wc"
                                          ; "arith.wc"
                                          ; "if.wc"
                                          ; "rec.wc"
                                          ]

(** Tells which method to run *)
type process = TeX
             | Shell
             | PAst
             | PCore
             | PAlpha
             | PMoved
             | PEta
             | PSuper
             | PPseudo


let textarea ?style ?onkeyup ?(attrs = []) children =
   let m = Html.create "textarea" ~attrs:attrs () in
      Html.set_attr_opt m "style" style ;
      Html.register_event_opt m "onkeyup" (match onkeyup with
                                            | None -> None
                                            | Some f -> Some (fun () -> f m)
                                          ) ;
      List.iter (Node.append m) children ;
      m

let updateText m =
 let s = Node.get_attribute m "value"
 in  Node.replace_all m (Node.text s)

let camlzone    = get_element_by_id "camlzone"   ;;
let inputcode   = textarea ~onkeyup:updateText ~attrs:[ ("id"      ,"inputcode" )
                                                      ; ("rows"    ,"30"        )
                                                      ; ("cols"    ,"60"        )
                                                      ]
                        [ Node.text ( "(* !!!!!! Type here your code !!!!!\n   For example:\n *)\n\n let f = fun x -> (x 7) in f\n") ]

let outputcode  = textarea                     ~attrs:[ ("id"      ,"outputcode")
                                                      ; ("rows"    ,"30"        )
                                                      ; ("cols"    ,"90"        )
                                                      ; ("readonly","yes"      )
                                                      ]
                        [ Node.text ( " The result will appear here! ") ]

let status      = Html.div ~attrs:[("id","status")] []
let setStatus s = Node.replace_all status (Node.text s) ;;

let loadfile s () =
 let _   = setStatus ("Loading file " ^ s ^ " ...") in
 let str = http_get s                               in
 let _   = setStatus  "Creating text node ..."      in
 let n   = Node.text str                            in
 let _   = setStatus  "Replacing input ..."         in
 let _   = Node.replace_all inputcode n             in
 let _   = setStatus  "Setting value ..."           in
 let _   = Node.set_attribute inputcode "value" str in
 setStatus "Done!"

let loadbutton s  = Html.a ~onclick:(loadfile s) [Html.string ("Load " ^ s ^ ".")] ;;
let loadoption s  = Html.option ~onclick:(loadfile s) [Html.string (s)] ;;

let output s = Node.replace_all outputcode (Node.text s) ;;


let process a =
 let _          = setStatus "Fetching input child ..."  in
 let child      = Node.child inputcode 0                in
 let _          = setStatus "Input to xml string ..."   in
 let str        = xml_of_dom child                      in
 let _          = setStatus "Creating lexing ..."       in
 let lex        = Lexing.from_string str                in
 let _          = setStatus "Parsing ast ..."           in
 let ast        = Parser.ast Lexer.lexer lex            in
 let texget   s = http_get ("tex/"   ^ s ^ ".tex")      in
 let shellget s = http_get ("shell/" ^ s         )      in
 let _          = setStatus "Compiling ...."            in
 let out        = (match a with
                     | TeX     -> processTeX    texget    ast
                     | Shell   -> processShell  shellget  ast
                     | PAst    -> processAst              ast
                     | PCore   -> processCore             ast
                     | PAlpha  -> processAlpha            ast
                     | PMoved  -> processMoved            ast
                     | PEta    -> processEta              ast
                     | PSuper  -> processSuper            ast
                     | PPseudo -> processPseudo           ast     
                  ) in
 let _          = setStatus "Creating output text node ..." in
 let text       = Node.text out                             in
 let _          = setStatus "Outputing ..."                 in
 let _          = Node.replace_all outputcode text
 in setStatus "Done!"



let optionTeX    = Html.option ~onclick:(fun () -> process TeX     ) [Html.string "TeX"                       ] ;;
let optionShell  = Html.option ~onclick:(fun () -> process Shell   ) [Html.string "Shell"                     ] ;;
let optionAst    = Html.option ~onclick:(fun () -> process PAst    ) [Html.string "Abstract-Syntax Tree"      ] ;;
let optionCore   = Html.option ~onclick:(fun () -> process PCore   ) [Html.string "Core intermediate language"] ;;
let optionAlpha  = Html.option ~onclick:(fun () -> process PAlpha  ) [Html.string "Alpha renamed Core"        ] ;;
let optionMoved  = Html.option ~onclick:(fun () -> process PMoved  ) [Html.string "Moved variable Core"       ] ;;
let optionEta    = Html.option ~onclick:(fun () -> process PEta    ) [Html.string "Eta reduced Core"          ] ;;
let optionSuper  = Html.option ~onclick:(fun () -> process PSuper  ) [Html.string "Super Combinators"         ] ;;
let optionPseudo = Html.option ~onclick:(fun () -> process PPseudo ) [Html.string "Pseudo code"               ] ;;


Node.append camlzone ( Html.div [
  Html.select (Html.option [Html.string "Choose a file to load"      ] :: (List.map loadoption examples)) ;
  Html.select [ Html.option [Html.string "Choose a compilation method"]
              ; optionTeX
              ; optionShell
              ; optionAst
              ; optionCore
              ; optionAlpha
              ; optionMoved
              ; optionEta
              ; optionSuper
              ; optionPseudo
              ] ;
  status ;
  Html.br () ;
  inputcode ;
  outputcode
] ) ;;


