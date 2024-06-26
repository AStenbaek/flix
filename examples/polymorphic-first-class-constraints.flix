/// Returns a set of edge facts labelled with numbers.
/// Note that the return type is `closed` which means that the
/// facts can *only* be used within a constraint system that
/// has labelled edges and paths of ints.
def getEdgesWithNumbers(): #{ LabelEdge(String, Int32, String), LabelPath(String, Int32, String) } = #{
    LabelEdge("a", 1, "b").
    LabelEdge("b", 1, "c").
    LabelEdge("c", 2, "d").
}

/// Returns a set of edge facts labelled with colors (strings).
/// Note that the return type is `open` (polymorphic) which
/// means that the facts can be used within any constraint
/// as long as the edges are labelled with strings.
def getEdgesWithColor(): #{ LabelEdge(String, String, String) | r } = #{
    LabelEdge("a", "red", "b").
    LabelEdge("b", "red", "c").
    LabelEdge("c", "blu", "d").
}

/// Returns a set of polymorphic rules to compute the transitive
/// closure of edges with the *same* label.
def getRules(): #{ LabelEdge(String, l, String), LabelPath(String, l, String) } with Order[l] = #{
    LabelPath(x, l, y) :- LabelEdge(x, l, y).
    LabelPath(x, l, z) :- LabelPath(x, l, y), LabelPath(y, l, z).
}

/// Computes the fixpoint of the two sets of facts with the rules.
/// Note that polymorphism allow us to use `getRules` with both types of facts.
def _f(): Unit =
    let _r1 = solve getEdgesWithColor() <+> getRules();
    let _r2 = solve getEdgesWithNumbers() <+> getRules();
    ()

/// However, the type system ensures that we do not mix facts of
/// different type:
def _g(): Unit =
    // Uncomment to see that the composition does not type check:
    // let _r1 = solve getEdgesWithColor() <+> getEdgesWithNumbers();
    ()
