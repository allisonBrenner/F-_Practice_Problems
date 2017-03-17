<Query Kind="FSharpProgram" />

//Question: Convert a binary search tree to a sorted double-linked list. 
//We can only change the target of pointers, but cannot create any new nodes.
//For example, if we input a binary search tree as shown on the left side of Fig 1, 
//the output double-linked list is shown on the right side.

//      (10)
//      /   \
//   (6)    (14)       --->   (4)<->(6)<->(8)<->(10)<->(12)<->(14)<->(16)
//   / \    /  \
// (4) (8) (12)(16)

open System

type BinaryTree =
    {
        root: Node option
    }
and Node = 
    {
        value: int
        leftChild: Node option
        rightChild: Node option
    }
    
type DoubleLinkedList =
    {
        first: Link option
        last: Link option
    }
and Link = 
    {
        value: int
        prevPointer: Link option
        nextPointer: Link option
    }
    
let node6 = {value = 16; leftChild = None; rightChild = None}
let node5 = {value = 12; leftChild = None; rightChild = None}
let node4 = {value = 8; leftChild = None; rightChild = None}
let node3 = {value = 4; leftChild = None; rightChild = None}
let node2 = {value = 14; leftChild = Some node5; rightChild = Some node6}
let node1 = {value = 6; leftChild = Some node3; rightChild = Some node4}
let node0 = {value = 10; leftChild = Some node1; rightChild = Some node2}
let tree = {root = Some node0}

let append (list:DoubleLinkedList) (value:int) =
    let newLink = 
        {
            value = value
            prevPointer = Some (list.last)
            nextPointer = None
        }   
    
    let newList = 
        {
            first = Some (list.first)
            last = Some (newLink)
        }
    
    newList

let makeLinkedList tree =
    let myList = {first = None; last = None}
    
    let rec traverse node list =
        
        match node.leftChild with
        | Some n -> traverse n list
        | None -> append list n.value
       
        match node.rightChild with
        | Some n -> traverse n list
        | None -> append list n.value
        
        list
        
    traverse tree.root myList
    myList
    
let answer = makeLinkedList tree