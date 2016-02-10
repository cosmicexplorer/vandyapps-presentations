CSS Selector Engine: Object Oriented Parsing
============================================

# What is a tree?

One definition says a tree is a graph with a single path between every node.

Ways to think about a tree:
- each node has one parent
- each node has many children

Which is more efficient? Implicit graph theorem discusses this a tad (check out Dr. Spinrad's book or his upper-level courses).

Typically, trees are just nodes. What are nodes?

1. Have a type
2. Have some data
3. Have some child nodes (or not)

This is a recursive structure!

# Why do we use trees?

To represent hierarchical data! e.g. this file!

# "Aren't hash tables more efficient?"

- complexity matters, but it depends upon the task
- hash tables don't have any inherent structure!
    - they're just flat arrays
- what does "structure" mean?

# You can query trees!

- HTML is a tree!
- CSS is used to modify the values of multiple html nodes
    - it does this DECLARATIVELY, not IMPERATIVELY
    - you don't tell it how to do find things, you just tell it what the results should look like
    - lot less error prone, can be optimized

# Example Selectors

- see tree on whiteboard

``` css
// css selectors are a single line
p a // links inside paragraphs "a tags inside p tags"
// not directly within!


p > a // links DIRECTLY within paragraphs


// more fun
:root > body h1 + p // what does this do?
```

# How does this turn into real code?

See above example: `:root > body h1 + p`.

We transform this into a function which will traverse a tree object to find these elements (if they exist). Higher order functions are very useful for this!

- `:root`
    - becomes a function which finds the root element of the tree (trivial)
    - pseudoclass selector
- `> body`
    - finds an element named `body` IMMEDIATELY below the element found by the previous selector.
    - immediate descendant combinator
- ` h1`
    - finds an element named `h1` somewhere below the `body` element found by the previous selector
    - descendant selector
- `+ p`
    - find an element named `p` IMMEDIATELY next to the `h1` tag found by the previous selector
    - adjacent sibling combinator

We can use higher order functions for this!

# Performance

Which way to store a tree is more efficient? How would you write functions implementing the above selectors?
