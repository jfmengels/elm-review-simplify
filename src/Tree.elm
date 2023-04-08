module Tree exposing (Tree(..), element, elementAlter, isLeaf, leaf, parts, tree)


type Tree element
    = Tree { element : element, parts : List (Tree element) }


leaf : element -> Tree element
leaf treeElement =
    tree treeElement []


tree : element -> List (Tree element) -> Tree element
tree treeElement treeParts =
    Tree
        { element = treeElement
        , parts = treeParts
        }


element : Tree element -> element
element (Tree treeInfo) =
    treeInfo.element


parts : Tree element -> List (Tree element)
parts (Tree treeInfo) =
    treeInfo.parts


isLeaf : Tree element -> Bool
isLeaf tree_ =
    List.isEmpty (parts tree_)


elementAlter : (element -> element) -> Tree element -> Tree element
elementAlter elementChange tree_ =
    tree (elementChange (element tree_)) (parts tree_)
