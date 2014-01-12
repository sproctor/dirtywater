open Template
open Template_collection

let () =
  let rock_template = new simple_template [] "rock" "a rock" "A small rock"
  in
  templates#add "rock" rock_template
