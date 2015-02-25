import java.util
import scala.collection.JavaConversions._
import scala.collection.mutable
import scala.collection.mutable.ListBuffer


object RegToNFA extends App {
  print("Enter regExp: ")
  var dfa = NFA.determinize(NFA.regExpToNFA(NFA.expandRegExpInput(scala.io.StdIn.readLine())))

  println("DFA: ")
  println(dfa)

  println("\"@reg %regExp%\" builds new automata")
  println("\"@exit\" closes")
  println("any other words gets checked")
  var work = true
  while (work) {
    val line = scala.io.StdIn.readLine()
    if (line == "@exit") {
      work = false
    } else if (line.startsWith("@reg")) {
      dfa = NFA.determinize(NFA.regExpToNFA(NFA.expandRegExpInput(line.split("\\s")(1))))
      println("DFA: ")
      println(dfa)
    } else {
      println(line + " - " + (if (DFA.check(dfa,0,line)) "accepted" else "error"))
    }
  }
}

class Edge(val from : Int, val to : Int, val symbol : Char)

class DFA() extends NFA() {
  val finalStates = new mutable.HashSet[Int]()

  override def toString(): String = {
    var result : String = ""
    for (trans : Edge <- edges) {
      result += "q" + trans.from + " => " + "q" + trans.to + " : " + trans.symbol + "\n"
    }
    result += "FS: "
    for (fs <- finalStates) {
      result += "q" + fs + " "
    }

    result
  }
}

object DFA {

  def check(dfa : DFA, stateId : Int, str : String): Boolean = {
    if (str.length == 0 && dfa.finalStates.contains(stateId)) {
      true
    } else if (str.length == 0) {
      false
    } else {
      var next = -1
      var i = 0
      while (next == -1 && i < dfa.edges.size) {
        //dfa, only one possible next state
        if (dfa.edges(i).from == stateId && dfa.edges(i).symbol == str.charAt(0)) {
          next = dfa.edges(i).to
        }
        i += 1
      }
      if (next != -1) check(dfa, next, str.tail) else false
    }
  }
}

class NFA() {

  val edges : util.ArrayList[Edge] = new util.ArrayList[Edge]()
  var finalState : Int = 0
  var size = 0

  def setSize(s : Int): Unit = size = s

  def getSize() : Int = size

  def addEdge(edge : Edge) = edges.add(edge)

  def addEdge(from : Int, to : Int, symbol : Char) = edges.add(new Edge(from,to,symbol))

  def setFinalState(state : Int) = finalState = state

  override def toString(): String = {
    var result : String = ""
    for (trans : Edge <- edges) {
      result += "q" + trans.from + " => " + "q" + trans.to + " : " + trans.symbol + "\n"
    }
    result += "FS: q" + finalState
    result
  }
}

object NFA {

  def isValue(ch : Char): Boolean = {
    ch >= 'a' && ch <= 'z'
  }

  def expandRegExpInput(regExp : String) : String = {
    if (regExp.length < 2) return regExp+'$'
    val x = regExp.charAt(0)
    val y = regExp.charAt(1)
    if ((isValue(x) || x == ')' || x == '*' || x == '?' || x == '+') && (isValue(y) || y == '(')) {
      x + "." + expandRegExpInput(regExp.tail)

    } else {
      x + expandRegExpInput(regExp.tail)
    }
  }

  def regExpToNFA(regExp : String) : NFA = {
    val operators : mutable.Stack[Char] = new mutable.Stack[Char]()
    val operands : mutable.Stack[NFA] = new mutable.Stack[NFA]()

    for (ch : Char <- regExp) {
      if (isValue(ch)) {
        operands.push(value(ch))
      } else {
        ch match {
          case '*' => operands.push(kleene(operands.pop()))
          case '?' => operands.push(maybe(operands.pop()))
          case '+' =>
            val a = operands.pop()
            operands.push(concat(a,kleene(a)))
          case '.' => operators.push(ch)
          case '|' => operators.push(ch)
          case '(' => operators.push(ch)
          case '/' => operators.push(ch)
          case ')' =>
            //make backtrack operations until "("
            while (operators.top != '(') {
              operators.pop() match {
                case '.' =>
                  val a = operands.pop()
                  operands.push(concat(operands.pop(), a))
                case '|' => operands.push(or(operands.pop(), operands.pop()))
              }
            }
            operators.pop() //popping "("
          case '$' =>
            //backtrack until operators stack is empty
            while (operators.nonEmpty) {
              operators.pop() match {
                case '.' =>
                  val a = operands.pop()
                  operands.push(concat(operands.pop(), a))
                case '|' => operands.push(or(operands.pop(), operands.pop()))
                case '/' =>
                  var a = operands.pop()
                  a = concat(operands.pop(), a)
                  operands.push(or(value('~'),a))
              }
            }
        }
      }
    }

    operands.top
  }

  def value(ch : Char): NFA = {
    val result = new NFA()
    result.setSize(2)
    result.addEdge(0,1,ch)
    result.setFinalState(1)
    result
  }

  def maybe(nfa : NFA) : NFA = {
    val result = new NFA()
    result.setSize(nfa.getSize() + 2)
    result.addEdge(0,1,'~')

    for (edge : Edge <- nfa.edges) {
      result.addEdge(edge.from+1, edge.to+1, edge.symbol)
    }

    result.addEdge(nfa.getSize(), nfa.getSize()+1, '~')
    result.addEdge(0, nfa.getSize()+1, '~')

    result.setFinalState(nfa.getSize()+1)
    result
  }

  def kleene(nfa : NFA) : NFA = {
    val result = new NFA()
    result.setSize(nfa.getSize() + 2)
    result.addEdge(0,1,'~')

    for (edge : Edge <- nfa.edges) {
      result.addEdge(edge.from+1, edge.to+1, edge.symbol)
    }

    result.addEdge(nfa.getSize(), nfa.getSize()+1, '~')
    result.addEdge(nfa.getSize(), 1, '~')
    result.addEdge(0, nfa.getSize()+1, '~')

    result.setFinalState(nfa.getSize()+1)
    result
  }

  def concat(nfaA : NFA, nfaB : NFA): NFA = {
    val result = new NFA()
    result.setSize(nfaA.getSize() + nfaB.getSize())

    for (edge : Edge <- nfaA.edges) {
      result.addEdge(edge.from,edge.to,edge.symbol)
    }

    result.addEdge(nfaA.finalState, nfaA.getSize(), '~')

    for (edge : Edge <- nfaB.edges) {
      result.addEdge(edge.from + nfaA.getSize(), edge.to + nfaA.getSize(), edge.symbol)
    }

    result.setFinalState(nfaA.getSize() + nfaB.getSize() - 1)

    result
  }

  def or(nfaA : NFA, nfaB : NFA): NFA = {
    val result = new NFA()
    result.setSize(2 + nfaA.getSize() + nfaB.getSize())

    result.addEdge(0,1,'~')

    for (edge : Edge <- nfaA.edges) {
      result.addEdge(edge.from + 1, edge.to + 1, edge.symbol)
    }

    result.addEdge(nfaA.finalState + 1, result.getSize() - 1, '~')

    result.addEdge(0,1+nfaA.getSize(),'~')

    for (edge : Edge <- nfaB.edges) {
      result.addEdge(edge.from + 1 + +nfaA.getSize(), edge.to + 1 + +nfaA.getSize(), edge.symbol)
    }

    result.addEdge(nfaB.finalState + 1 +nfaA.getSize(), result.getSize() - 1, '~')

    result.setFinalState(result.getSize() - 1)
    result
  }

  def eClosure(nfa : NFA, stateId : Int) : mutable.ListBuffer[Int] = {
    val result = mutable.ListBuffer[Int]()
    val visited = new Array[Boolean](nfa.getSize())
    for (i <- 0 until visited.size) {
      visited(0) = false
    }
    val nodeQueue = new mutable.Queue[Int]()

    nodeQueue.enqueue(stateId)
    result.add(stateId)
    while (nodeQueue.nonEmpty) {
      val curr = nodeQueue.dequeue()
      visited(curr) = true
      for (edge : Edge <- nfa.edges) {
        if (edge.from == curr && edge.symbol == '~') {
          if (!visited(edge.to)) {
            nodeQueue.enqueue(edge.to)
            result.add(edge.to)
          }
        }
      }


    }


    result.sorted
  }

  def eClosure(nfa : NFA, stateId : mutable.Set[Int]) : mutable.ListBuffer[Int] = {
    var result = mutable.ListBuffer[Int]()
    for (id <- stateId) {
      result = result.union(eClosure(nfa, id))
    }
    result.sorted
  }

  def move(nfa : NFA, from : ListBuffer[Int], ch : Char) : mutable.Set[Int] = {
    val result = mutable.Set[Int]()
    for (edge : Edge <- nfa.edges) {
      if (edge.symbol == ch && from.contains(edge.from)) {
        result.add(edge.to)
      }
    }
    result
  }

  def getAlphabet(nfa : NFA) : mutable.Set[Char] = {
    val result = mutable.Set[Char]()

    for (edge : Edge <- nfa.edges) {
      if (isValue(edge.symbol)) {
        result.add(edge.symbol)
      }
    }

    result
  }

  def determinize(nfa : NFA) : DFA = {
    val nodeMap = new mutable.HashMap[mutable.ListBuffer[Int],Int]()
    val alphabet = getAlphabet(nfa)
    val processQueue = new mutable.Queue[mutable.ListBuffer[Int]]
    val result = new DFA()

    //add eClosure of start to our nodeMap
    processQueue.enqueue(eClosure(nfa,0))
    nodeMap(eClosure(nfa,0)) = 0

    while (processQueue.nonEmpty) {
      val curr = processQueue.dequeue()
      for (ch <- alphabet) {
        val target = eClosure(nfa,move(nfa,curr,ch))
        if (target.size > 0) {
          if (nodeMap.contains(target)) {
            //that node already exists just add edge
            result.addEdge(nodeMap(curr), nodeMap(target), ch)
          } else {
            //new node
            nodeMap(target) = nodeMap.size
            result.addEdge(nodeMap(curr), nodeMap(target), ch)
            processQueue.enqueue(target)
          }
        }
      }

    }

    for (s <- nodeMap.keySet) {

      if (s.contains(nfa.finalState)) {
        result.finalStates.add(nodeMap(s))
      }
    }

    result
  }
}