use graphviz_rs::prelude::Graph;
use rhai::CustomType;

use super::Ref;

impl CustomType for Ref<'static, Graph> {
    fn build(_builder: rhai::TypeBuilder<Self>) {
        todo!()
    }
}

#[derive(Debug, Clone)]
pub struct OwnedGraph(pub Graph);
impl CustomType for OwnedGraph {
    fn build(mut builder: rhai::TypeBuilder<Self>) {
        builder.with_name("Graph");

        ////////////////////////////////////////////////////////////////////////

        // builder
        //     .with_fn("new", |name: ImmutableString|)
    }
}
