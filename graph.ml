type 'a t = {nodes: 'a list; edges: ('a * 'a) list}

let empty () =
  {nodes=[]; edges=[]}

let add_node graph node =
  {graph with nodes=node::graph.nodes}

let add_edge graph from_node to_node =
  let edge = (from_node,to_node) in
  {graph with edges=edge::graph.edges}

let nodes graph =
  graph.nodes

let edges graph =
  graph.edges

let edges_from graph node =
  List.filter (fun (from_node,to_node) -> node == from_node) graph.edges

let edges_to graph node =
  List.filter (fun (from_node,to_node) -> node == to_node) graph.edges

let nodes_from graph node =
  let edges = edges_from graph node in
  List.map (fun (_,to_node) -> to_node) edges

let nodes_to graph node =
  let edges = edges_to graph node in
  List.map (fun (from_node,_) -> from_node) edges
