(*
#!/usr/bin/env ocaml
#use "topfind" ;;
#require "netclient" ;;
#require "xml-light" ;;
#require "extlib" ;;
*)

(* Code released under the MIT license:
   http://opensource.org/licenses/MIT
*)

let lang = "de"  (* German language *)
let lang = "fr"  (* French language *)


let exec_command command =
  let lines = ref "" in
  let chan = Unix.open_process_in command in
  try
    while true; do
      lines := !lines ^ (input_line chan) ^ "\n"
    done; !lines
  with End_of_file ->
    ignore(Unix.close_process_in chan);
    !lines


let match_regexp rex str =
  let regexp = Netstring_pcre.regexp rex in
  let results = Netstring_pcre.full_split regexp str in
  match results with
  | [Netstring_pcre.Text s] -> (false, [])
  | (Netstring_pcre.Delim s)::tail ->
      (true, List.map (fun elem -> match elem with
                       | Netstring_pcre.Group (nbr,st) -> st
                       | _ -> "") tail)
  | _ -> (false, [])


let replace ch ~pattern:regex ~by:remplacement =
  Netstring_pcre.global_replace (Netstring_pcre.regexp regex) remplacement ch


(* TODO retrun what we're searching, not the element *)
let rec collect_tree f lst tree =
  match tree with
  | Xml.Element (a , b, lst) as el ->
      if f el
      then el::(List.flatten(List.map (collect_tree f lst) lst))
      else List.flatten(List.map (collect_tree f lst) lst)
  | Xml.PCData a -> []


let rec find_item_videos el = match el with
  | Xml.Element("item", [],
      Xml.Element("title", [], [Xml.PCData title]) :: infos) ->
      (true, title, infos)
  | _ ->
      (false, "", [Xml.PCData ""])


let rec find_item_videos_bool el =
  match el with
  | Xml.Element("item", [],
      Xml.Element("title", [], [Xml.PCData title]) :: infos) -> true
  | _ -> false


let find_videos arb =
  collect_tree find_item_videos_bool [] arb


type arte_video = {
  title : string;
  description : string;
  urlHtml : string;
  date : string;
}



(* STEP 1 : BASE XML *)
let xmlListVideos () =
  let get  = Http_client.Convenience.http_get in
  let xml = get ("http://videos.arte.tv/" ^ lang ^ "/do_delegate/videos/\
    index--3188698,view,asCoverflowXml.xml?hash=/tv/coverflow///1/120/") in
  Xml.parse_string xml



(* STEP 1 : base xml, we search the url html to find the xml 2 *)
let find_element_video  el  =
  match el with
  | Xml.Element ("video", [] , b) -> true
  | _ -> false


let return_video_structure el =
  match el with
  | Xml.Element ("video", [],
      pgrmtype :: stycker ::
      (Xml.Element ("title", [], [Xml.PCData title])) ::
      (Xml.Element("teaserText", [],[Xml.PCData description])) ::
      imageurl ::
      (Xml.Element("targetUrl",[],[Xml.PCData link])) ::
      (Xml.Element("startDate",[],[Xml.PCData date])) :: remain
    ) ->
      {
        title       = replace title ~pattern:"&#039;" ~by:"'";
        description = replace description ~pattern:"&#039;" ~by:"'";
        urlHtml     = "http://videos.arte.tv" ^ link;
        date        = date;
      }
  | _ ->
      {
        title       = "";
        description = "";
        urlHtml     = "";
        date        = "";
      }



let construct_video_list () =
  List.map
    return_video_structure
    (collect_tree find_element_video [] (xmlListVideos ()))


let display_video_list () =
  ExtList.List.iteri
    (fun nbr elem ->
      let struc = return_video_structure elem in
      print_endline ("[" ^ (string_of_int nbr) ^ "]");
      print_endline ("\t" ^ struc.title);
      print_endline ("\t" ^ struc.description);
    )
    (collect_tree find_element_video [] (xmlListVideos ()))

let menu () =
  display_video_list ()


(* Does work *)
(* We're searching the XML 2 in the HTML *)
let get_url_rtmp_from url =
  let get = Http_client.Convenience.http_get in
  let htmlbrut = get url in
  let html = replace htmlbrut ~pattern:"[\\t\\n]" ~by:"" in
  let html2 = replace html ~pattern:".*videorefFileUrl" ~by:"videorefFileUrl" in
  let xml1brutUrl =
    let a, b = match_regexp "videorefFileUrl\\s*=\\s*\"(.+?)\"" html2 in
    List.hd b
  in
  let find_element_url_lang_fr el =
    match el with
    | Xml.Element( "video", [("lang", _lang);("ref", url)], [])
      when _lang = lang -> true
    | _ -> false
  in
  let find_element_url_rtmp_video el =
    match el with
    | Xml.Element ("url", [("quality", "hd")], (Xml.PCData url)::[]) -> true
    | _ -> false
  in
  let find_url_rtmp xml2 =
    let tag = List.hd (collect_tree find_element_url_rtmp_video [] xml2) in
    match tag with
    | Xml.Element ("url", [("quality", "hd")], (Xml.PCData url)::[]) -> url
    | _ -> ""
  in
  let xml1 =
    let brut = get xml1brutUrl in
    Xml.parse_string brut
  in
  let xml2 =
    let xml2_url =
      let elem = List.hd (collect_tree find_element_url_lang_fr [] xml1) in
      match elem with
      | Xml.Element("video", [("lang", _lang);("ref", url)], [])
        when _lang = lang -> url
      | _ -> ""
    in
    Xml.parse_string (get xml2_url)
  in
  find_url_rtmp xml2


(* STEP 2 : We get the xml 2 from html 1 *)
let get_url_rtmp_par_numero num =
  let video_list = construct_video_list () in
  get_url_rtmp_from (List.nth video_list num).urlHtml


let dump_by_number num =
  let video_list = construct_video_list () in
  let title      = replace (List.nth video_list num).title ~pattern:"['/]" ~by:" " in
  let url_rtmp   = get_url_rtmp_par_numero num in
  print_endline ("Téléchargement de " ^ title);
  (*
  exec_command ("rtmpdump  -e -r '" ^ url_rtmp ^ "' -o '" ^ title ^ ".flv'")
  *)
  exec_command ("rtmpdump  -e -r '" ^ url_rtmp ^ "' | mplayer -")


let dl = dump_by_number ;;


let i_am_interactive () =
  Unix.isatty Unix.stdin &&
  Unix.isatty Unix.stdout


let repl () =
  try
    menu ();
    while true do
      if i_am_interactive ()
      then print_string "Choisissez une vidéo - Quit ou ligne vide pour sortir : ";
      let line = read_line () in
      if (String.lowercase line) = "quit" || line = ""
      then raise End_of_file
      else dl (int_of_string line);
      print_endline "Téléchargement terminé";
      menu ();
      
      (* do something with the line *)
    done
  with End_of_file -> ()


let () =
  repl ()
