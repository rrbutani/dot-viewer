// fn filter(&Graph, curr_selection: &[&NodeId]) -> &[NodeId];
// fn modify(graph: &Graph, selection: Option<&[&NodeId]>) -> new_graph: Graph;

// if we're willing to simplify the API we can just provide only an OwnedGraph
// type, make copies, and eat the perf hit of having to clone the graph when
// calling `filter`...
//
// I think that's what we'll do; the duplication would be quite annoying
// otherwise
//
// need to make a note saying that any modifications that `filter` makes will
// not persist
//
// this also sidesteps some potential lifetime issues I think?

// fn filter(Graph, curr_selection: &[&NodeId]) -> &[NodeId];
// fn modify(graph: Graph, selection: Option<&[&NodeId]>) -> new_graph: Graph;

//////////////////////////////////////////////////////////////////////

// types:
//   - EdgeId
//   - NodeId
//   - SubgraphId
//
//   - Edge
//   - Node
//   - Subgraph
//   - Graph
//   - &Graph?

// for all:
//   - https://rhai.rs/book/rust/print-custom.html

// for `ArrayRef<T>` we'd need to implement these methods from [this list](https://rhai.rs/book/language/arrays.html#built-in-functions):
// (i.e. all the methods that don't mutate):
//  - `get`
//  - `+`
//  - `==`
//  - `!=`
//  - `pop`
//  - `shift`
//  - `extract(start)`
//  - `extract(start, num_elements)`
//  - `extract(range)`
//  - `len` (method and property)
//  - `is_empty` (method and property)
//  - `truncate`
//  - `chop`
//  - `split`
//  - `for_each`
//  - `filter`
//  - `contains`
//  - `index_of(elem)`
//  - `index_of(elem, start_pos)`
//  - `index_of(fn)`
//  - `index_of(fn, start_pos)`
//  - `find(fn)`
//  - `find(fn, start_pos)`
//  - `find_map(fn)`
//  - `find_map(fn, start_pos)`
//  - `map`
//  - `reduce(fn)`
//  - `reduce(fn, initial_val)`
//  - `reduce_rev(fn)`
//  - `reduce_rev(fn, initial_val)`
//  - `some`
//  - `all`
//
//  - indexing
//
//  - plus:
//    + `to_owned(&self) -> Array`

// for `HashMapRef<K, V>` we'd need these from [here](https://rhai.rs/book/language/object-maps.html#built-in-functions):
//  - `get` -- note: will have to clone
//  - `len`
//  - `is_empty`
//  - `==`
//  - `!=`
//  - `contains`
//    + may wish to do extras and also support builtin strings and stuff.. (if type is a `String` or `&str` or `Cow<str>` or such)
//      * might be hard though to do generically..
//  - `keys` -> ArrayRef<K> // nope, this would be owned..
//  - `values` -> ArrayRef<V> // nope, this would be owned..
//  - `to_json`
//    + actually, nah
//
//  - indexing
//
//  - and: `to_owned`

// actually I don't think we need `HashMapRef` at all..

// `HashSetRef<T>`: there isn't a native hashset type but... let's offer:
//  - `get`
//  - `len`
//  - `is_empty`
//  - `==`
//  - `!=`
//  - `contains`
//  - `keys`
//  - `to_owned` -> to native Object with values = ()

// NOTE: need to support the rhai-typed equivs in args in the above where possible...

// Graph (aka MutGraphWrapper)
//   - new(name: str) -> Graph
//   - new(name: str, nodes: HashSetRef<Node> | Array | ArrayRef<Node>) -> Graph
//   - new(name: str, nodes: HashSetRef<Node> | Array | ArrayRef<Node>, edges: HashSetRef<Edge> | Array | ArrayRef<Edge>) -> Graph
//   - clone(&mut self) -> Graph
//
//
//   - add_subgraph(&mut self)
//   - remove_nodes(&mut self)
//   - add_node(&mut self)
//   - remove_edges(&mut self)
//   - add_edge(&mut self)
//
//   // ugh, let's just do a clone for these (HashSet<Attr> types)
//   // I don't want to do a _mutable_ HashSet wrapper in addition to the other
//   // thing
//   //
//   // TODO(scripting): perf-optimization opportunity..
//   //
//   // note: nothing is pass by reference; the objectmap is cloned when we pass
//   // it in so we have to accept an objectmap as an output; i.e. we have no
//   // way to do `&mut ObjectMap`
//   - modify_subgraph_attrs(&self, func: fn(ObjectMap) -> ObjectMap) -> Option<()>
//   - modify_node_attrs(&self, func: fn(ObjectMap) -> ObjectMap) -> Option<()>
//   - modify_edge_attrs(&self, func: fn(ObjectMap) -> ObjectMap) -> Option<()>
//
//   // not sure if we want to bother with wrapper types for SubgraphId/NodeId/EdgeId
//   //
//   // it's maybe hassle to check for them everywhere to make stuff ergonomic?
//   // on the other hand it does make it so we can overload some interfaces
//   // easier
//   //
//   // I think we'll do it and offer a `to_string` function on the wrappers
//   // (instead of trying to recreate the whole String API on them..)
//   // String API for reference: https://rhai.rs/book/language/string-fn.html
//   // we'll at least do display and equality and stuff though
//
//   - (+ everything from &Graph below except `to_owned`)
//
//   - register_set? -- tricky with the subgraph/node/edge properties I think
//     + actually no, it's totally doable!
//     + we'd just need to check that on set the key isn't being changed I guess
//     + `Edge`, `Node`, and `Subgraph` should disallow setting `id`/`to`/`from`
//       * or we should catch ^ and do the "right thing"?
//         - ^ is what would be tricky
//
//   - assign

// &Graph (aka RefToGraph)
//   - contains(_) with {EdgeId, NodeId, SubgraphId}
//     + also have the `contains_{name}` variants too I guess...
//   - contains(_) with str (defaults to NodeId)
//   - index with {EdgeId, NodeId, SubgraphId} -> {_}
//   - index with str (defaults to NodeId)
//   - index with tuple is EdgeId
//
//   - register_get
//
//   - id(&self) -> &String
//
//   - subgraphs(&self) -> HashSetRef<Graph>
//   - nodes(&self) -> HashSetRef<NodeId>
//   - edges(&self) -> HashSetRef<EdgeId>
//
//   // since these are expensive anyways, directly produce the rhai types and lets do maps:
//   // (clone the elements too..)
//   //
//   // warn that these are expensive in the docs
//   //
//   // not sure there's a real use case for these though..
//   - subgraphs_as_map(&self) -> ObjectMap<SubgraphId, Subgraph>
//   - nodes_as_map(&self) -> ObjectMap<SubgraphId, Node>
//   - edges_as_map(&self) -> ObjectMap<SubgraphId, Edge>
//
//   - is_acyclic(&self)
//
//     // don't have a place to store the results; turn it into a rhai type,
//     // it's fine
//     //
//     // note that we're cloning the strings too (it's fine)
//   - topsort(&self) -> Array<NodeId>
//
//   // don't have a place to stick the resulting graph so we're forced to go
//   // the OwnedGraph route
//   - filter(&self, node_ids: impl Iterator<...>) -> OwnedGraph
//   - children(&self, root: NodeId | String | ..., depth: optional) -> OwnedGraph
//   - parents(&self, base: NodeId | String | ..., depth: optional) -> OwnedGraph
//   - neighbors(&self, center: NodeId | String | ..., depth: optional) -> OwnedGraph
//   - select_subset(&self, starting: NodeId | String | ..., directions: ??, depth: optional) -> OwnedGraph
//
//   - extract_subgraph(&self, subgraph: SubgraphId | String) -> OwnedGraph
//
//   // Don't want to make reference types for these, we'll just clone; not
//   // worth wrapping
//   - get_subgraph(&self, id: SubgraphId | String | ...) -> OwnedSubgraph
//   - get_node(&self, id: NodeId | String | ...) -> OwnedNode
//   - get_edge(&self, id: EdgeId | String | ...) -> OwnedEdge
//
//
//   // again: can't store the results of these so we're back to owned types:
//   - collect_subgraphs(&self, subgraph: SubgraphId | String) -> Array<SubgraphId>
//   - collect_nodes(&self, id: SubgraphId | String) -> Array<NodeId>
//   - collect_edges(&self, id: SubgraphId | String) -> Array<EdgeId>
//
//   - incoming(&self)
//     + (aka froms, aka predecessors)
//   - outgoing(&self)
//     + (aka tos, aka successors)
//
//   - to_dot(&self) -> String
//
//   - to_owned -> Graph

///////////////////////////////////////////////////////////////////////

// TODO: persist a scope for loaded commands
// higher optimization for `loaded`, lowered for `script`
// TODO: docs
// TODO: deploy docs on github actions
// TODO: vscode ext in workspace recs, rhai-doc and rhai-lsp in flake
// TODO: gitattributes in the repo for .rhai as rust
// TODO: custom operators

///////////////////////////////////////////////////////////////////////

use rhai::EvalAltResult;

type RhaiResult<T> = Result<T, Box<EvalAltResult>>;

mod array_ref;
