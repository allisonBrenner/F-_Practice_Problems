<Query Kind="FSharpProgram">
  <NuGetReference>FSharp.Data</NuGetReference>
  <NuGetReference>System.Data.SQLite</NuGetReference>
</Query>

open System

//                    0
//                  /    \
//                1       2
//               / \     / \
//              3   4   5   6
//             / \         /
//            7   8       9
//                         \
//                          10

type Node =
    {
        value: int
        leftChild: Node option
        rightChild: Node option
    }

let example10 = {value=10; leftChild=None; rightChild=None}
let example9 = {value=9; leftChild=None; rightChild= Some(example10)}
let example8 = {value=8; leftChild=None; rightChild=None}
let example7 = {value=7; leftChild=None; rightChild=None}
let example6 = {value=6; leftChild=Some(example9); rightChild=None}
let example5 = {value=5; leftChild=None; rightChild=None}
let example4 = {value=4; leftChild=None; rightChild=None}
let example3 = {value=3; leftChild=Some(example7); rightChild=Some(example8)}
let example2 = {value=2; leftChild=Some(example5); rightChild=Some(example6)}
let example1 = {value=1; leftChild=Some(example3); rightChild=Some(example4)}
let example0 = {value=0; leftChild=Some(example1); rightChild=Some(example2)}

//1.	Check if node is a leaf node 
let isLeaf node =
    match node.leftChild with
    | Some(x)-> false
    | None->
      match node.rightChild with
        | Some(x)-> false
        | None-> true

let leaf = sprintf "1.	Check if node is a leaf node -> %A" (isLeaf example10) |> Dump


//2.	Print all leaf nodes
//      [4;5;7;8;10]
let rec print (node:Node option) acc : 'a list=
    match node with
    | Some n ->
        let update = if isLeaf n then n.value::acc else acc
        print n.leftChild (print n.rightChild update)
    | None -> acc
    
   
let printed = sprintf "2.	Print all leaf nodes -> %A" (print (Some(example0)) []) |> Dump

//3.	Find the height of a tree (or given node)
//      base case, the node is a leaf node, it's height is 1 (or 0 depending how you want to count)
//      if not a leaf then, it is the height of its tallest child + 1
//      helper function 
let getMax a b =
    if a > b then
        a
    else
        b

//helper function
let abs a =
    if a < 0 then
        - 1 * a
    else
    a
    
let rec getHeight node =
    if isLeaf node then
        1
    else
        let lh =
            match node.leftChild with
            | Some x->getHeight x
            | None-> 1
        let rh =
            match node.rightChild with
            | Some x->getHeight x
            | None-> 1
        let max = getMax lh rh
        max + 1

let height = sprintf "3.	Find the height of a tree -> %A" (getHeight example0) |> Dump


//4.	Check if a tree is balanced
//      base case, a tree with no children is balanced
//      otherwise the tree is balanced if the difference in height between the left & right side of the tree is less than 2
//      meaning each subtree is balanced - so will need to get height and balanced flag for each side
let rec isBalanced node =
    if isLeaf node then
        true
    else
        let lh =
            match node.leftChild with
            | Some x-> (getHeight x, isBalanced x)
            | None-> (1, true)
        let rh =
            match node.rightChild with
            | Some x-> (getHeight x, isBalanced x)
            | None-> (1, true)
        match(lh, rh) with
        | ((l,true),(r,true)) ->
            match abs l - r with
            | x when x< 2 -> true
            | _ -> false
        | _ -> false
        
        
let balanced = sprintf "4.	Check if a tree is balanced -> %A" (isBalanced example0) |> Dump

//5.	Check if a tree is a binary search tree
//      Definition. A binary search tree (BST) is a binary tree where each node has a Comparable key (and an associated value) 
//      and satisfies the restriction that the key in any node is larger than the keys in all nodes in that node's left subtree 
//      and smaller than the keys in all nodes in that node's right subtree.

//------------------- BT ---------------//--------------- BST ------------
//                    0                 //                 5
//                  /    \              //              /     \     
//                1       2             //            3        9
//               / \     / \            //           / \      / \
//              3   4   5   6           //          2   4    7   10
//             / \         /            //         /        / \  
//            7   8       9             //        1        6   8
//---------------- (values) ------------//------------- (keys) ------------

type BST =
    {
        key: int
        value: string
        left: BST option
        right: BST option
    }

let bst8 = {key=8; value="lazy"; left=None; right=None}
let bst6 = {key=6; value="over"; left=None; right=None}
let bst1 = {key=1; value="the"; left=None; right=None}
let bst10 = {key=10; value="dog"; left=None; right=None}
let bst7 = {key=7; value="the"; left=Some(bst6); right=Some(bst8)}
let bst4 = {key=4; value="fox"; left=None; right=None}
let bst2 = {key=2; value="quick"; left=Some(bst1); right=None}
let bst9 = {key=9; value="yellow"; left=Some(bst7); right=Some(bst10)}
let bst3 = {key=3; value="brown"; left=Some(bst2); right=Some(bst4)}
let bst5 = {key=5; value="jumps"; left=Some(bst3); right=Some(bst9)}

let rec isBinarySearch (node:BST option)=  
    match node with
    | Some root ->
        let leftKeyLess =
            match root.left with
            | Some l -> l.key < root.key
            | None -> true
        
        let rightKeyMore =
            match root.right with
            | Some r -> r.key > root.key
            | None -> true
    
        match (leftKeyLess, rightKeyMore, (isBinarySearch root.left), (isBinarySearch root.right)) with
        | (true, true, true, true) -> true
        | _ -> false
        
    | None -> true

        

let binSearch = sprintf "5.	Check if a tree is a binary search tree -> %A" (isBinarySearch (Some(bst5))) |> Dump

//--------------- BST ------------
//              jumps
//             /     \     
//        brown       yellow
//        /   \       /     \
//     quick   fox   the     dog
//     /            /   \  
//   the          over  lazy
//--------------------------------

//6.	Traverse a binary tree using in-order traversal
//      If a tree is a binary search tree then an in-order traversal will print in increasing order
//      recursively print the left subtree, print the root, print the right subtree
//      LEFT > ROOT > RIGHT
//      the quick brown fox jumps over the lazy yellow dog
let rec inOrder (node:BST option) (starter:string) : string=
    match node with
    | Some root -> 
        let leftstr = inOrder root.left ""
        let middlestr = root.value + " "
        let rightstr = inOrder root.right ""
        leftstr + middlestr + rightstr
    | None -> starter
        

let ioTraverse = sprintf "6.	Traverse a binary tree using in-order traversal -> %A" (inOrder (Some(bst5)) "") |> Dump 

//7.	Traverse a binary tree using pre-order traversal
//      Pre-order would copy a subtree because it starts at the root, then left, then right
//      ROOT > LEFT > RIGHT
//      jumps brown quick the fox yellow the over lazy dog
let rec preOrder (node:BST option) (starter:string) : string=
    match node with
    | Some root -> 
        let leftstr = inOrder root.left ""
        let middlestr = root.value + " "
        let rightstr = inOrder root.right ""
        middlestr + leftstr + rightstr + " "
    | None -> starter

let preTraverse = sprintf "7.	Traverse a binary tree using pre-order traversal -> %A" (preOrder(Some(bst5)) "") |> Dump

//8.	Traverse a binary tree using post-order 
//      For post order, start with the left subtree, then right, then root
//      LEFT > RIGHT > ROOT
//      the quick fox brown over lazy the dog yellow jumps
let rec postOrder (node:BST option) (starter:string) : string=
    match node with
    | Some root -> 
        let leftstr = inOrder root.left ""
        let middlestr = root.value + " "
        let rightstr = inOrder root.right ""
        leftstr + rightstr + middlestr + " "
    | None -> starter

let postTraverse = sprintf "8.	Traverse a binary tree using post-order traversal -> %A" (postOrder (Some(bst5)) "") |> Dump