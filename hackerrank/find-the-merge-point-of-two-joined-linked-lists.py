"""
 Find the node at which both lists merge and return the data of that node.
 head could be None as well for empty list
 Node is defined as

 class Node(object):

   def __init__(self, data=None, next_node=None):
       self.data = data
       self.next = next_node
"""

def FindMergeNode(headA, headB):
    nodes = set()
    while headA != None:
        nodes.add(headA)
        headA = headA.next

    while headB != None:
        if headB in nodes:
            return headB.data
        else:
            headB = headB.next
