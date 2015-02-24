import java.util
import scala.collection.JavaConversions._
import scala.collection.mutable

object RegToNFA extends App {

  val regExp = "a?"
  println(NFA.expandRegExpInput(regExp))
  println(NFA.regExpToNFA(NFA.expandRegExpInput(regExp)))


}

class Edge(val from : Int, val to : Int, val symbol : Char)

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
      x + '.' + expandRegExpInput(regExp.tail)

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
          case '.' => operators.push(ch)
          case '|' => operators.push(ch)
          case '(' => operators.push(ch)
          case ')' =>
            //make backtrack operations until "("
            while (operators.top != '(') {
              operators.pop() match {
                case '.' => operands.push(concat(operands.pop(), operands.pop()))
                case '|' => operands.push(or(operands.pop(), operands.pop()))
              }
            }
          case '$' =>
            //backtrack until operators stack is empty
            while (operators.nonEmpty) {
              operators.pop() match {
                case '.' => operands.push(concat(operands.pop(), operands.pop()))
                case '|' => operands.push(or(operands.pop(), operands.pop()))
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

  def determinize(nfa : NFA) : NFA = {
    null
  }
}