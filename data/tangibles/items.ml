open Template
open Template_collection

let () =
  let rock_template = new simple_template [] "rock" "a rock" "A small rock."
  in templates#add "rock" rock_template;
  let table_template = new container_template [] "table" "a table" "A rickety-looking table."
  in templates#add "table" table_template;
  let plate_template = new simple_template ["dinner"] "plate" "a dinner plate" "A chipped dinner plate that looks it has been passed down through multiple generations." in
  templates#add "plate" plate_template;
  let note_template = new simple_template [] "note" "a note" "A note with an apparently hastily scribbled message on it." in
  templates#add "note" note_template
